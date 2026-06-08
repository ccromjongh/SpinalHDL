package spinal.core.pdg_analysis

/**
  * This file defines the data structures for representing a Program Dependency Graph (PDG) for a circuit.
  * Original author: Jarl Brand, 2025 for Chisel
  */

import java.util.UUID

sealed trait VertexKind

object VertexKind {
    case object Definition extends VertexKind
    case object DataDefinition extends VertexKind
    case object IO extends VertexKind
    case object Connection extends VertexKind
    case object ControlFlow extends VertexKind
}

sealed trait EdgeKind

object EdgeKind {
    case object Data extends EdgeKind
    case object Conditional extends EdgeKind
    case object Declaration extends EdgeKind
    case object Index extends EdgeKind
}

case class PDGCondition(
    probeName: Seq[String],
    probeValue: Seq[Int]
)

case class PDGVertex(
    file: String,
    line: Int,
    char: Int,
    name: String,
    kind: VertexKind,
    clocked: Boolean,
    modulePath: Seq[String],
    relatedSignal: Option[(String, String)] = None, // Hierarchical path to signal, field path to individual node
    condition: Option[PDGCondition] = None,
    assignsTo: Option[String] = None,
    isChiselStatement: Boolean = true, // Indicates that the statement refers to an original chisel statement (so no nodes/wires/regs with _ prefix)
    assignDelay: Int = 0, // Enables support for sequential memories
    uid: String = UUID.randomUUID().toString // To allow hashing of otherwise identical vertices
)

case class PDGEdge(
    from: PDGVertex,
    to: PDGVertex,
    kind: EdgeKind,
    clocked: Boolean,
    condition: Option[PDGCondition] = None
)

case class PDGEdgeSerializable(
    from: Int,
    to: Int,
    kind: EdgeKind,
    clocked: Boolean,
    condition: Option[PDGCondition] = None
)

case class ExportableCFGNode(
    stmtRef: Int, // This should be the index of the PDGVertex that references the actual statement
    predStmtRef: Option[Int], // In case the node is a fork, this reference should point to the predicate probe signal
    trueBranch: Option[Seq[ExportableCFGNode]],
    falseBranch: Option[Seq[ExportableCFGNode]]
)
case class ProgramDependencyGraph(
    vertices: Seq[PDGVertex],
    edges: Seq[PDGEdgeSerializable],
    predicates: Seq[PDGVertex],
    cfg: Seq[ExportableCFGNode]
)

case class ProgramDependencyGraphJson(filename: String, graph: ProgramDependencyGraph) {

  def getBytes: String = {
    val verticesJson = graph.vertices.map { v =>
      s"""{
         |  "file": "${v.file}",
         |  "line": ${v.line},
         |  "char": ${v.char},
         |  "name": "${v.name}",
         |  "kind": "${v.kind.toString}",
         |  "clocked": ${v.clocked},
         |  "modulePath": [${v.modulePath.mkString("\"", "\", \"", "\"")}],
         |  "relatedSignal": ${
        v.relatedSignal.map { case (signalPath, fieldPath) =>
          s"""{
             |  "signalPath": "$signalPath",
             |  "fieldPath": "$fieldPath"
             |}""".stripMargin
        }.getOrElse("null")
      },
         |  "assignsTo": ${v.assignsTo.map(x => s""""$x"""").getOrElse("null")},
         |  "isChiselStatement": ${v.isChiselStatement},
         |  "condition": ${
        v.condition.map { c =>
          s"""{
             |  "probeName": [${c.probeName.mkString("\"", "\", \"", "\"")}],
             |  "probeValue": [${c.probeValue.mkString(", ")}]
             |}""".stripMargin
        }.getOrElse("null")
      },
         |  "assignDelay": ${v.assignDelay}
         |}""".stripMargin
    }.mkString("[", ",", "]")

    val edgesJson = graph.edges.map { e =>
      s"""{
         |  "from": ${e.from},
         |  "to": ${e.to},
         |  "kind": "${e.kind.toString}",
         |  "clocked": ${e.clocked},
         |  "condition": ${
        e.condition.map { c =>
          s"""{
             |  "probeName": [${c.probeName.mkString("\"", "\", \"", "\"")}],
             |  "probeValue": [${c.probeValue.mkString(", ")}]
             |}""".stripMargin
        }.getOrElse("null")
      }
         |}""".stripMargin
    }.mkString("[", ",", "]")

    val predicatesJson = graph.predicates.map { p =>
      s"""{
         |  "file": "${p.file}",
         |  "line": ${p.line},
         |  "char": ${p.char},
         |  "name": "${p.name}",
         |  "kind": "${p.kind.toString}",
         |  "clocked": false,
         |  "isChiselStatement": false
         |}""".stripMargin
    }.mkString("[", ",", "]")

    val cfgJson = getCFGJsonRecursive(graph.cfg)

    val outString =
      s"""{
         |  "vertices": $verticesJson,
         |  "edges": $edgesJson,
         |  "predicates": $predicatesJson,
         |  "cfg": $cfgJson
         |}""".stripMargin

    outString
  }

    private def getCFGJsonRecursive(nodes: Seq[ExportableCFGNode]): String = {
        nodes.map { node =>
            // Always include the stmtRef field
            val stmtRefJson = s""""stmtRef": ${node.stmtRef}"""

            // Conditionally include the predStmtRef field if defined
            val predStmtRefJson = node.predStmtRef match {
                case Some(pred) => s""""predStmtRef": $pred"""
                case _ => ""
            }
            
            // Conditionally include the trueBranch field if defined and non-empty
            val trueBranchJson = node.trueBranch match {
            case Some(branch) if branch.nonEmpty => s""""trueBranch": ${getCFGJsonRecursive(branch)}"""
            case _ => ""
            }
            
            // Conditionally include the falseBranch field if defined and non-empty
            val falseBranchJson = node.falseBranch match {
            case Some(branch) if branch.nonEmpty => s""""falseBranch": ${getCFGJsonRecursive(branch)}"""
            case _ => ""
            }
            
            // Collect all non-empty fields and join them with commas
            val fields = Seq(stmtRefJson, predStmtRefJson, trueBranchJson, falseBranchJson).filter(_.nonEmpty).mkString(", ")
            s"{$fields}"
        }.mkString("[", ", ", "]")
    }
}
