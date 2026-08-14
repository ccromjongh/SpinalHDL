package spinal.core.pdg_analysis

import spinal.core.internals._
import spinal.core._
import spinal.core.fiber.Handle.initImplicit
import spinal.core.pdg_analysis.PdgBuilder._
import spinal.idslplugin.Location

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class BuildPdgPhase extends PhaseMisc {
  var random = new Random(seed = 123456L)

  override def impl(pc: PhaseContext): Unit = {
    import pc._
    
    val startTime = System.nanoTime()

    var componentList: Vector[Component] = Vector.empty
    pc.topLevel.walkComponents { comp =>
        componentList = componentList :+ comp
    }

    // Now, we add nodes that will allow us to read the predicate signals from a VCD
    val modulePredMap = scala.collection.mutable.HashMap[String, Map[String, String]]()
    createProbes(pc.topLevel, modulePredMap)

    val (verts, edges, cfg) = buildPDG(componentList, pc.topLevel.definitionName, Seq.empty)

    val vertMap = verts.zipWithIndex.toMap
    val serializableEdges = edges.map(e => PDGEdgeSerializable(vertMap(e.from), vertMap(e.to), e.kind, e.clocked, e.condition))

    /*pc.topLevel.walkComponents { comp =>
      comp.dslBody.walkLeafStatements {
        case t: BaseType => {
          val isPred = t.name.startsWith("when_")
          if (isPred) {
            modulePredMap(comp.definitionName) = modulePredMap.getOrElse(comp.definitionName, Map.empty) + (t.name -> t.name)
          }
          print(t)
        }
        case _ =>
      }
    }*/


    val numVerts = verts.length
    val predicateVerts = scala.collection.mutable.ArrayBuffer[PDGVertex]()
    // Now that we have inserted some nodes that we can probe, we need to transform the CFG for export
    def makeCFGExportable(s: Seq[CFGNode]): Seq[ExportableCFGNode] = {
      s.flatMap {
        case CFGStatement(stmt) => {
          // This here is needed because during the matching stage, wrongly inferred memory ports may be removed,
          // but they are still present in the CFG. This makes sure they don't make it to the final version
          vertMap.get(stmt.vertex) match {
            case None => Seq.empty
            case Some(vert) => Seq(ExportableCFGNode(vert, None, None, None, None, None))
          }
        }
        case CFGFork(stmt, predSignalName, hierPrefix, left, right) => {
          vertMap.get(stmt.vertex) match {
            case None => Seq.empty
            case Some(vert) => {
              // First, get the name of the probe node
              val predNodeName = modulePredMap(stmt.sourceModule)(predSignalName)

              // Now, we convert the relative name to a hierarchical name
              val hierPredNodeName = prefixSymbol(predNodeName, hierPrefix)

              // Insert it into the list of statements, or if it already exists, get the index
              var predIdx = predicateVerts.indexWhere(v => v.name == hierPredNodeName)
              if (predIdx == -1) {
                predIdx = predicateVerts.length
                predicateVerts.append(PDGVertex(stmt.vertex.file, stmt.vertex.line, stmt.vertex.char, hierPredNodeName, VertexKind.DataDefinition, false, Seq.empty))
              }
//              var predIdx = 0

              Seq(ExportableCFGNode(vert, Some(predIdx), Some(makeCFGExportable(left)), Some(makeCFGExportable(right)), None, None))
            }
          }
        }
        case CFGMultiFork(stmt, predSignalName, hierPrefix, branches, defaultBranch) => {
          vertMap.get(stmt.vertex) match {
            case None => Seq.empty
            case Some(vert) => {
              // First, get the name of the probe node
              val predNodeName = modulePredMap(stmt.sourceModule)(predSignalName)

              // Now, we convert the relative name to a hierarchical name
              val hierPredNodeName = prefixSymbol(predNodeName, hierPrefix)

              // Insert it into the list of statements, or if it already exists, get the index
              var predIdx = predicateVerts.indexWhere(v => v.name == hierPredNodeName)
              if (predIdx == -1) {
                predIdx = predicateVerts.length
                predicateVerts.append(PDGVertex(stmt.vertex.file, stmt.vertex.line, stmt.vertex.char, hierPredNodeName, VertexKind.DataDefinition, false, Seq.empty))
              }

              val branchNodes = branches.map(branch => ExportableCFGBranch(branch.file, branch.line, branch.char, branch.matchValues, makeCFGExportable(branch.stmts)))
              val defaultBranchNode = ExportableCFGBranch(defaultBranch.file, defaultBranch.line, defaultBranch.char, defaultBranch.matchValues, makeCFGExportable(defaultBranch.stmts))
              Seq(ExportableCFGNode(vert, Some(predIdx), None, None, Some(branchNodes), Some(defaultBranchNode)))
            }
          }
        }
      }
    }

    val exportableCFG = makeCFGExportable(cfg)

    val pdg = ProgramDependencyGraph(verts.toSeq, serializableEdges, predicateVerts.toSeq, exportableCFG)
    val pdgJSON = ProgramDependencyGraphJson("pdg", pdg).getBytes

    val endTime = System.nanoTime()
    val duration = (endTime - startTime) / 1e6d
    println(f"PDG construction completed in $duration%.1f ms.")

    val targetPath = pc.config.targetDirectory + "/" + topLevel.definitionName + ".json"
    val outFile = new java.io.FileWriter(targetPath)
    outFile.write(pdgJSON)
    outFile.close()
    println(f"PDG JSON file written to $targetPath.")

    val debugJSON =
      s"""{
         |  "cfg": [
         |    ${cfg.map(_.toJSON).mkString(", ")}
         |  ],
         |  "predicates": [
         |    ${predicateVerts.map(_.toJSON).mkString(", ")}
         |  ],
         |  "verts": [
         |    ${verts.map(_.toJSON).mkString(", ")}
         |  ],
         |  "edges": [
         |    ${edges.map(_.toJSON).mkString(", ")}
         |  ]
         |}""".stripMargin

    val targetDebugPath = pc.config.targetDirectory + "/" + topLevel.definitionName + "-debug.json"
    val outDebugFile = new java.io.FileWriter(targetDebugPath)
    outDebugFile.write(debugJSON)
    outDebugFile.close()
  }

  private def generateRandomString(length: Int): String = {
    val letters = ('a' to 'z').mkString + ('A' to 'Z').mkString
    (1 to length).map(_ => letters(random.nextInt(letters.length))).mkString
  }

  def createProbes(comp: Component, modulePredMap: scala.collection.mutable.HashMap[String, Map[String, String]]): Unit = {
    var compPredMap: Map[String, String] = Map.empty

    comp.dslBody.walkStatements {
      case x: AssignmentStatement =>
        x.source match {
          case binMult: BinaryMultiplexer =>
            val loc = x.sourceLocation
            val predName = "mux_" + loc.fileSymbol + "_l" + loc.line
            val proxy = Bool()
            proxy.setName(predName)
            proxy.setRefOwner(comp)
            proxy.parentScope = x.parentScope
            proxy.setLocation(x.sourceLocation)

            val assign = DataAssignmentStatement(proxy, binMult.cond)
            if (x.sourceLocation != null) {
              assign.setLocation(x.sourceLocation)
            }

            x.insertNext(proxy)
            proxy.insertNext(assign)
            binMult.cond = proxy

            compPredMap += predName -> predName
          case _ =>
        }
      // When statements always already have a predicate. Either it is a Boolean expression, or it will have been swapped by a `when_file_l123` type proxy signal.
      case wstmt: WhenStatement =>
        val condition = wstmt.cond.asInstanceOf[Bool]
        val conditionName = condition.getName()
        compPredMap += conditionName -> conditionName
      case sstmt: SwitchStatement =>
        val condition = sstmt.value.asInstanceOf[BaseType]
        val conditionName = condition.getName()
        compPredMap += conditionName -> conditionName
      /*case sstmt: SwitchStatement =>
        // Important: predicates for conditional statements are *not* probes, the GUI trace app will treat them differently.
        // This is, as I understand it, not a design choice but something that happened through the agile nature of a thesis during development.
        // Todo: see if this can be turned into a more semantic name. I can use the conditionName, but what about duplicates?
        val parent = sstmt.parentScope
        val sig = sstmt.value.asInstanceOf[BaseType]
        val sigName = sig.name
        for (branch <- sstmt.elements) {
          for (branchKey <- branch.keys) {
            val predName = "pred_" + generateRandomString(10)
            val name: String = branchKey match {
              case enum: EnumLiteral[_] => enum.senum.getName()
              case bt: BaseType => bt.getName()
            }
            val proxy = Bool()
            proxy.setName(predName)
            proxy.setRefOwner(comp)
            proxy.parentScope = parent
            proxy.setLocation(sstmt.sourceLocation)

            sig match {
              case craft: SpinalEnumCraft[_] =>
                comp.rework {
                  val bitWidth = craft.getBitsWidth bits
                  val craftSig = craft.wrapCast(Bits(bitWidth), new CastEnumToBits)
                  craftSig.setName("enumSig_" + generateRandomString(10))
                  branchKey match {
                    case enum: EnumLiteral[_] =>
                      val value = B(enum.getValue(), bitWidth)
                      value.setName("enumLiteral_" + generateRandomString(10))
                      val someSigValue = value === craftSig
                      someSigValue.setName("enumComp_" + generateRandomString(10))
                      print(someSigValue)
                      val litSig = enum.senum.craft(craft.getEncoding)
                      litSig.setName("enumCraft_" + generateRandomString(10))
                      val secondComp = litSig.isEqualTo(craft)
                      secondComp.setName("enumComp2_" + generateRandomString(10))
                  }
                }
            }

            compPredMap += name -> predName
          }
        }*/
      case _ =>
    }
    modulePredMap(comp.definitionName) = compPredMap

    comp.children.foreach(_.walkComponents(nested => createProbes(nested, modulePredMap)))
  }
}
