package spinal.core.pdg_analysis

import spinal.core.internals._
import spinal.core._
import spinal.core.fiber.Handle.initImplicit
import spinal.core.pdg_analysis.PdgBuilder._

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
            case Some(vert) => Seq(ExportableCFGNode(vert, None, None, None))
          }
        }
        case CFGFork(stmt, predSignalName, hierPrefix, left, right) => {
          vertMap.get(stmt.vertex) match {
            case None => Seq.empty
            case Some(vert) => {
              // First, get the name of the newly inserted probe node
              val predNodeName = modulePredMap(stmt.sourceModule)(predSignalName)
              // dontTouchAnnos.append(DontTouchAnnotation(ComponentName(predNodeName, ModuleName(stmt.sourceModule, CircuitName(circuit.main)))))

              // Now, we convert the relative name to a hierarchical name
              val hierPredNodeName = prefixSymbol(predNodeName, hierPrefix)

              // Insert it into the list of statements, or if it already exists, get the index
              var predIdx = predicateVerts.indexWhere(v => v.name == hierPredNodeName)
              if (predIdx == -1) {
                predIdx = predicateVerts.length
                predicateVerts.append(PDGVertex(stmt.vertex.file, stmt.vertex.line, stmt.vertex.char, hierPredNodeName, VertexKind.DataDefinition, false, Seq.empty))
              }
//              var predIdx = 0

              Seq(ExportableCFGNode(vert, Some(predIdx), Some(makeCFGExportable(left)), Some(makeCFGExportable(right))))
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
  }

  private def generateRandomString(length: Int): String = {
    val letters = ('a' to 'z').mkString + ('A' to 'Z').mkString
    (1 to length).map(_ => letters(random.nextInt(letters.length))).mkString
  }

  def createProbes(comp: Component, modulePredMap: scala.collection.mutable.HashMap[String, Map[String, String]]): Unit = {
    val targets = ArrayBuffer[WhenStatement]()
    var compPredMap: Map[String, String] = Map.empty

    comp.dslBody.walkStatements {
      case cond: WhenStatement => targets += cond
      case _ =>
    }

    for (cond <- targets) {
      val condition = cond.cond
      val parent = cond.parentScope

      val proxy = Bool()
      val name = "probe_" + generateRandomString(10)
      proxy.setName(name)
      proxy.setRefOwner(comp)
      proxy.parentScope = parent
      proxy.setLocation(cond.sourceLocation)

      val assign = InitAssignmentStatement(proxy, condition)
      if (cond.sourceLocation != null) {
        assign.setLocation(cond.sourceLocation)
      }

      cond.insertNext(proxy)
      proxy.insertNext(assign)
      cond.cond = proxy
      compPredMap += name -> name
    }
    modulePredMap(comp.definitionName) = compPredMap

    comp.children.foreach(_.walkComponents(nested => createProbes(nested, modulePredMap)))

    /*var compPredMap: Map[String, String] = Map.empty
    comp.dslBody.walkStatements {
      case cond: WhenStatement => {
        val condition = cond.cond
        val isPred = condition.name.startsWith("when_")
        if (isPred) {
          compPredMap += condition.name -> condition.name
        } else {
          val newStatement = Bool()
          newStatement := condition
          val name = "probe_" + generateRandomString(10)
          newStatement.setName(name)
          compPredMap += name -> name
          cond.parentScope.head.insertNext(newStatement)
        }
      }
    }*/
  }
}
