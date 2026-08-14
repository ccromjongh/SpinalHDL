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
) {
  def toJSON: String =
    s"""{
       |  "probeName": [${this.probeName.mkString("\"", "\", \"", "\"")}],
       |  "probeValue": [${this.probeValue.mkString(", ")}]
       |}""".stripMargin
}

/**
 *
 * @param file File this symbol resides in
 * @param line Line of the symbol in the file
 * @param char Column of the symbol in the line
 * @param name Unique identifier for the symbol. This name is used for statements to depend on. Will be displayed in the graph viewer as the node's label.
 * @param kind The type, or roll, of this symbol
 * @param clocked Is this element clocked? I.e., is it a register?
 * @param modulePath Hierarchical path to module
 * @param relatedSignal Hierarchical path to signal, field path to individual node
 * @param condition Todo: figure out when this is used
 * @param assignsTo Signal that this symbol assigns to, not set for conditionals
 * @param isChiselStatement Indicates that the statement refers to an original chisel statement (so no nodes/wires/regs with _ prefix)
 * @param assignDelay Enables support for sequential memories
 * @param uid To allow hashing of otherwise identical vertices
 */
case class PDGVertex(
    file: String,
    line: Int,
    char: Int,
    name: String,
    kind: VertexKind,
    clocked: Boolean,
    modulePath: Seq[String],
    relatedSignal: Option[(String, String)] = None,
    condition: Option[PDGCondition] = None,
    assignsTo: Option[String] = None,
    isChiselStatement: Boolean = true,
    assignDelay: Int = 0,
    uid: String = UUID.randomUUID().toString
) {
  def toJSON: String =
    s"""{
       |  "file": "${this.file}",
       |  "line": ${this.line},
       |  "char": ${this.char},
       |  "name": "${this.name}",
       |  "kind": "${this.kind.toString}",
       |  "clocked": ${this.clocked},
       |  "modulePath": [${this.modulePath.mkString("\"", "\", \"", "\"")}],
       |  "relatedSignal": ${
      this.relatedSignal.map { case (signalPath, fieldPath) =>
        s"""{
           |  "signalPath": "$signalPath",
           |  "fieldPath": "$fieldPath"
           |}""".stripMargin
      }.getOrElse("null")
    },
       |  "assignsTo": ${this.assignsTo.map(x => s""""$x"""").getOrElse("null")},
       |  "isChiselStatement": ${this.isChiselStatement},
       |  "condition": ${
      this.condition.map { c =>
        s"""{
           |  "probeName": [${c.probeName.mkString("\"", "\", \"", "\"")}],
           |  "probeValue": [${c.probeValue.mkString(", ")}]
           |}""".stripMargin
      }.getOrElse("null")
    },
       |  "assignDelay": ${this.assignDelay}
       |}""".stripMargin
}

case class PDGEdge(
    from: PDGVertex,
    to: PDGVertex,
    kind: EdgeKind,
    clocked: Boolean,
    condition: Option[PDGCondition] = None
) {
  def toJSON: String =
    s"""{
       |  "from": ${this.from.toJSON},
       |  "to": ${this.to.toJSON},
       |  "kind": "${this.kind.toString}",
       |  "clocked": ${this.clocked},
       |  "condition": ${this.condition.map(_.toJSON).getOrElse("null")}
       |}""".stripMargin
}

case class PDGEdgeSerializable(
    from: Int,
    to: Int,
    kind: EdgeKind,
    clocked: Boolean,
    condition: Option[PDGCondition] = None
) {
  def toJSON: String =
    s"""{
       |  "from": ${this.from},
       |  "to": ${this.to},
       |  "kind": "${this.kind.toString}",
       |  "clocked": ${this.clocked},
       |  "condition": ${this.condition.map(_.toJSON).getOrElse("null")}
       |}""".stripMargin
}

case class ExportableCFGNode(
    stmtRef: Int, // This should be the index of the PDGVertex that references the actual statement
    predStmtRef: Option[Int], // In case the node is a fork, this reference should point to the predicate probe signal
    trueBranch: Option[Seq[ExportableCFGNode]],
    falseBranch: Option[Seq[ExportableCFGNode]],
    branches: Option[Seq[ExportableCFGBranch]],
)

case class ExportableCFGBranch(
    matchValues: Seq[String],
    stmts: Seq[ExportableCFGNode]
)

case class ProgramDependencyGraph(
    vertices: Seq[PDGVertex],
    edges: Seq[PDGEdgeSerializable],
    predicates: Seq[PDGVertex],
    cfg: Seq[ExportableCFGNode]
)

case class ProgramDependencyGraphJson(filename: String, graph: ProgramDependencyGraph) {

  def getBytes: String = {
    val verticesJson = graph.vertices.map(_.toJSON).mkString("[", ",", "]")

    val edgesJson = graph.edges.map(_.toJSON).mkString("[", ",", "]")

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

            // Conditionally include the falseBranch field if defined and non-empty
            val branchesJson = node.branches match {
            case Some(branches) if branches.nonEmpty => s""""branches": [${branches.map(b => {
              s"""{"matchValues": [${b.matchValues.map('"' + _ + '"').mkString(", ")}], "stmts": ${getCFGJsonRecursive(b.stmts)}}"""
            }).mkString(", ")}]"""
            case _ => ""
            }
            
            // Collect all non-empty fields and join them with commas
            val fields = Seq(stmtRefJson, predStmtRefJson, trueBranchJson, falseBranchJson, branchesJson).filter(_.nonEmpty).mkString(", ")
            s"{$fields}"
        }.mkString("[", ", ", "]")
    }
}
