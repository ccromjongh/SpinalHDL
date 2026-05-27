package spinal.core.pdg_analysis

import spinal.core.internals._
import spinal.core._
import spinal.core.pdg_analysis.PdgBuilder._

class BuildPdgPhase extends PhaseMisc {
  override def impl(pc: PhaseContext): Unit = {
    import pc._
    
    val startTime = System.nanoTime()

    var componentList: Vector[Component] = Vector.empty
    pc.topLevel.walkComponents { comp =>
        componentList = componentList :+ comp
    }

    val (verts, edges, cfg) = buildPDG(componentList, pc.topLevel.definitionName, Seq.empty)

    val vertMap = verts.zipWithIndex.toMap
    val serializableEdges = edges.map(e => PDGEdgeSerializable(vertMap(e.from), vertMap(e.to), e.kind, e.clocked, e.condition))

    // Now, we add nodes that will allow us to read the predicate signals from a VCD
    val modulePredMap = scala.collection.mutable.HashMap[String, Map[String, String]]()

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
              /*// First, get the name of the newly inserted probe node
              val predNodeName = modulePredMap(stmt.sourceModule)(predSignalName)
              // dontTouchAnnos.append(DontTouchAnnotation(ComponentName(predNodeName, ModuleName(stmt.sourceModule, CircuitName(circuit.main)))))

              // Now, we convert the relative name to a hierarchical name
              val hierPredNodeName = prefixSymbol(predNodeName, hierPrefix)

              // Insert it into the list of statements, or if it already exists, get the index
              var predIdx = predicateVerts.indexWhere(v => v.name == hierPredNodeName)
              if (predIdx == -1) {
                predIdx = predicateVerts.length
                predicateVerts.append(PDGVertex(stmt.vertex.file, stmt.vertex.line, stmt.vertex.char, hierPredNodeName, VertexKind.DataDefinition, false, Seq.empty))
              }*/
              val predIdx = 0

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
}
