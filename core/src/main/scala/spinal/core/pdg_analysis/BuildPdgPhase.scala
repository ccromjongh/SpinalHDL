package spinal.core.pdg_analysis

import spinal.core.internals._
import spinal.core._
import spinal.core.pdg_analysis.PdgBuilder.buildPDG

class BuildPdgPhase extends PhaseMisc {
  override def impl(pc: PhaseContext): Unit = {
    import pc._
    
    val startTime = System.nanoTime()

    val componentList: Vector[Component] = Vector.empty
    pc.topLevel.walkComponents { comp =>
        componentList :+ comp
    }

    val (vertexes, edges, nodes) = buildPDG(componentList, pc.topLevel.name, Seq.empty)

    val endTime = System.nanoTime()
    val duration = (endTime - startTime) / 1e6d
    println(f"PDG construction completed in $duration%.1f ms.")
  }
}
