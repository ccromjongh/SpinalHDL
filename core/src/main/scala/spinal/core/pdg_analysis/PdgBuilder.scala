package spinal.core.pdg_analysis

import scala.util.Random
import scala.collection.mutable.{HashMap, ArrayBuffer, Set}
import spinal.core._
import spinal.core.internals._
import spinal.core.pdg_analysis.VertexKind._

/**
  * This file contains the methods for constructing a Program Dependency Graph (PDG) for a circuit.
  * Adapted from Jarl Brand's code for Chisel.
  */

object PdgBuilder {

  // --------------------------------------------- //
  // ----- Data storage for PDG construction ----- //
  // --------------------------------------------- //

  // Initialize a global random number generator to produce the same probe names each time.
  var random = new Random(seed = 123456L)

  // Used to detect module paths in symbols
  val modulePrefixes = Set[String]()

  // Used for type inference of compound signals
  // Hier-name -> (Type, module, isIOandShouldBeFlipped)
  val compoundSignals: HashMap[String, (SignalType.Value, String, Boolean)] = HashMap.empty



  // --------------------------------------------- //
  // ------------- Utility functions ------------- //
  // --------------------------------------------- //

  private def generateRandomString(length: Int): String = {
    val letters = ('a' to 'z').mkString + ('A' to 'Z').mkString
    (1 to length).map(_ => letters(random.nextInt(letters.length))).mkString
  }

  // TODO ADAPT2SPINAL
  /// Gets the hierarchical module path from the prefix string
  /// All it does is split the string by "." to get a sequence of parts
  /// However, we wish to export this information for grouping, so this is the best way to do it
  def getModulePath(prefix: String): Seq[String] = prefix match {
    case null | "" => Seq.empty
    case s if s.contains('.') => s.split('.').toSeq
    case s => Seq(s)
  }

  def getSourceLocation(stmt: Statement): (String, Int, Int) = {
    val l = stmt.sourceLocation
    if (l != null) {
      (l.file + ".scala", l.line, l.col)
    } else {
      ("unknown_file", 0, 0)
    }
  }

  def prefixSymbol(symbol: String, prefix: String): String = {
    if (prefix.isBlank()) {
      symbol
    } else {
      prefix + "." + symbol
    }
  }
  
  def prefixStrings(symbols: Seq[String], prefix: String): Seq[String] = {
    if (prefix.isBlank()) {
      symbols
    } else {
      symbols.map(s => prefix + "." + s)
    }
  }

  def prefixSymbols(symbols: Vector[PDGDependency], prefix: String): Vector[PDGDependency] = {
    if (prefix.isBlank()) {
      symbols
    } else {
      symbols.map{
        case r: RegularDependency => r.copy(name = prefix + "." + r.name, rootName = prefix + "." + r.rootName)
        case c: ConditionalDependency => c.copy(name = prefix + "." + c.name, rootName = prefix + "." + c.rootName, conditionSignals = prefixStrings(c.conditionSignals, prefix))
      }
    }
  }

  def parseLocation(input: String): (String, Int) = {
    // Split the input on spaces: ["@", "BaseType.scala", "l306"]
    val parts = input.split(" ")

    val fileName = parts(1)
    val lineNumber = parts(2).drop(1).toInt

    (fileName, lineNumber)
  }

  def insertVectorProbes(root: Component): Unit = {
    root.walkComponents { comp =>
      val probes = ArrayBuffer[String]()

      /** Replaces all index operations with references to a (to be created) probe signal in an expression
        * Returns the new expression and a map containing the probe names, along with their desired values.
        * These values may themselved contain vector indexing, so they should be recursively processed to take out
        * all vector indexing.
        */
      def replaceSubAccessIndices(expr: Expression): (Expression, Map[String, Expression]) = {
        val replaceMap = HashMap[String, Expression]()
        // TODO
        val newExpr = expr
        (newExpr, replaceMap.toMap)
      }

      /// Extracts dynamic vector index operations to probe signals
      def extractVectorIndices(stmt: Statement): Seq[Statement] = {
        // TODO
        Seq.empty
      }
    }
  }



  // --------------------------------------------- //
  // ------ Data types for PDG construction ------ //
  // --------------------------------------------- //

  object SignalType extends Enumeration {
    type SignalType = Value
    val Ground, Bundle, Vector = Value
  }

  // TODO: Check if an unspecified direction is possible within SpinalHDL.
  sealed trait InferDirection
  case object InferWrite extends InferDirection
  case object InferRead extends InferDirection

  /**
   *
   * @param vertex Definition of the symbol and its properties
   * @param sourceModule The module that the statement is from
   * @param dependencies The symbols that the statement depends on
   * @param provides The symbols that the statement provides. This is used to in conjunction with the dependencies to form edges.
   * @param clocked Is this element clocked? I.e., is it a register?
   * @param isInferredMemoryDirection In Chisel, memories can have an unspecified direction, which needs to be inferred
   */
  case class ConnectableStatement(
    vertex: PDGVertex,
    sourceModule: String,
    dependencies: Seq[PDGDependency],
    provides: Seq[PDGDependency],
    clocked: Boolean,
    isInferredMemoryDirection: Option[InferDirection] = None
  )

  sealed trait CFGNode

  // The implicit assumption is that the graph is stored in a sequence and that sequential elements are connected.
  // This vastly simplifies the datastructure that is needed to store the graph and allows for easier graph generation.
  case class CFGStatement(
    stmt: ConnectableStatement
  ) extends CFGNode

  /**
   * Represents a condition in the circuit in the CFG
   * @param stmt The properties of this signal node
   * @param predSignalName Signal name in VCD with the value of the predicate
   * @param hierPrefix Hierarchy prefix of signal within VCD
   * @param left The 'true' branch of a conditional
   * @param right The alternative branch
   */
  case class CFGFork(
    stmt: ConnectableStatement,
    predSignalName: String,
    hierPrefix: String,
    left: Seq[CFGNode],
    right: Seq[CFGNode]
  ) extends CFGNode

  sealed trait PDGDependency {
    def name = ""
    def rootName = ""
    def connectID = ""
    def flipped = false

    def joinIfNotEmpty(a: String, b: String): String = {
      if (a.nonEmpty && b.nonEmpty) s"$a.$b"
      else if (a.nonEmpty) a
      else b
    }

    def combine(other: PDGDependency): PDGDependency = (this, other) match {
      case (r1: RegularDependency, r2: RegularDependency) => {
        RegularDependency(joinIfNotEmpty(r1.name, r2.name), r1.rootName, r1.flipped ^ r2.flipped, joinIfNotEmpty(r1.connectID, r2.connectID))
      }
      case (c1: ConditionalDependency, r1: RegularDependency) => {
        c1.copy(name=joinIfNotEmpty(c1.name, r1.name), flipped= c1.flipped ^ r1.flipped, connectID = joinIfNotEmpty(c1.connectID,r1.connectID))
      }
      case (r1: RegularDependency, c1: ConditionalDependency) => {
        c1.copy(name=joinIfNotEmpty(r1.name, c1.name), flipped= c1.flipped ^ r1.flipped, rootName = r1.rootName, connectID = joinIfNotEmpty(r1.connectID,c1.connectID))
      }
      case (c1: ConditionalDependency, c2: ConditionalDependency) => {
        ConditionalDependency(
          joinIfNotEmpty(c1.name, c2.name),
          c1.rootName,
          c1.flipped ^ c2.flipped,
          joinIfNotEmpty(c1.connectID, c2.connectID),
          c1.conditionSignals ++ c2.conditionSignals,
          c1.conditionValues ++ c2.conditionValues
        )
      }
    }
  }
  // Dependencies between statements can either be normal (RegularDependency), or conditional.
  // A conditional dependency in this case means a vector that is indexed dynamically: the condition is that the index signal has a certain value.

  case class RegularDependency(
    override val name: String,
    override val rootName: String,
    // Used for flipped connections in bundles. See spec. If a connection is flipped, the signal flow direction is reversed.
    // -> Flipped inputs become outputs, lhs becomes rhs if rhs signal is flipped.
    // Technically, the flip direction needs to be the same for connections, however we don't need to worry, because the compiler will
    // handle all validity checks
    override val flipped: Boolean,
    // The connectID is used to replicate the FIRRTL connection algorithm (see spec)
    override val connectID: String = "",
    isIndex: Boolean = false
  ) extends PDGDependency

  case class ConditionalDependency(
    override val name: String,
    override val rootName: String,
    override val flipped: Boolean,
    override val connectID: String,
    conditionSignals: Seq[String],
    conditionValues: Seq[Int]
  ) extends PDGDependency

  sealed trait CompoundSignalPathNode
  case class DynamicVectorIndex(signalName: String) extends CompoundSignalPathNode
  case class StaticVectorIndex(idx: Int) extends CompoundSignalPathNode
  case class BundleField(name: String) extends CompoundSignalPathNode



  // ---------------------------------------------- //
  // ---------------- Main methods ---------------- //
  // ---------------------------------------------- //

  case class DefTypeAlias() // TODO: implement type aliases if they are used in SpinalHDL. This is currently a placeholder to avoid having to refactor the code that is already using this type alias.

  /// Main entry point for building a PDG.
  def buildPDG(modules: Seq[Component], mainModule: String, typeAliases: Seq[DefTypeAlias]): (Vector[PDGVertex], Vector[PDGEdge], Vector[CFGNode]) = { 
    // Todo, figure out if the type aliases are used in SpinalHDL.
    // [Jarl] This is using singleton and then clearing the globals at the start of building a PDG.
    // [Jarl] TODO: this is the first thing that needs to be refactored, it should have been a temp fix.
    compoundSignals.clear()
    modulePrefixes.clear()
    random = new Random(seed = 123456L)
    // First, make a map of the module by name for referencing the instances
    val moduleMap = modules.map(m => (m.definitionName, m)).toMap
    val statements = findDependenciesRecursive(moduleMap, mainModule, typeAliases)

    val (verts, edges) = matchDependencies(statements)
    (verts, edges, statements)
  }

  def findDependenciesRecursive(moduleMap: Map[String, Component], root: String, typeAliases: Seq[DefTypeAlias], isMain: Boolean = true, prefix: String = "", moduleDep: Option[PDGDependency] = None): Vector[CFGNode] = {
    // Output can contain bundles that might contain flipped direction signals, so we need to split them into an Input and Output
    // part. Every item in a bundle will have its own vertex
    val module = moduleMap(root)

    def prefixStrings(symbols: Seq[String]): Seq[String] = {
      if (prefix.isBlank()) {
        symbols
      } else {
        symbols.map(s => prefix + "." + s)
      }
    }

    def prefixSymbols(symbols: Vector[PDGDependency]): Vector[PDGDependency] = {
      if (prefix.isBlank()) {
        symbols
      } else {
        symbols.map{
          case r: RegularDependency => r.copy(name = prefix + "." + r.name, rootName = prefix + "." + r.rootName)
          case c: ConditionalDependency => c.copy(name = prefix + "." + c.name, rootName = prefix + "." + c.rootName, conditionSignals = prefixStrings(c.conditionSignals))
        }
      }
    }

    val modulePath = getModulePath(prefix)

    val ioStatements: Vector[CFGStatement] = Vector.empty
    /*val ioStatements =
      module.ioSet.flatMap {p =>
        val (file, line, col) = getSourceLocation(p)
        // Todo: SPINAL check inout and other special types
        p.dir match {
          case spinal.core.in => {
            if (p.name == "clock") { // Filter out the clock signal => for explanation, see extractStatements()
              Vector.empty
            } else if (p.name == "reset") {
              val relatedSignal = Some((prefixSymbol("reset", prefix), ""))
              Vector(CFGStatement(ConnectableStatement(
                PDGVertex(file, line, col, s"input_reset", VertexKind.IO, false, modulePath, relatedSignal, None, Some(prefixSymbol("reset", prefix)), true),
                root,
                Vector.empty,
                prefixSymbols(Vector(RegularDependency(p.name, p.name, false))),
                false
              )))
            } else {
              //if (addCompoundSignal(prefixSymbol(p.name, prefix), p.tpe, prefix, false, typeAliases)) {
              if (true) {
                val compoundDefs = getSingleDeps(p, Seq.empty, typeAliases, Some(p.name), p.name + ".")
                compoundDefs.map{d => 
                  if (d.flipped) {
                    // It's an output node
                    CFGStatement(ConnectableStatement(
                      PDGVertex(file, line, col, s"output_${d.name}", VertexKind.IO, false, modulePath, isChiselStatement = true),
                      root,
                      prefixSymbols(Vector(d)),
                      Vector.empty,
                      false
                    ))
                  } else {
                    // Input node
                    val relatedSignal = {
                      val fieldPathPrefix = d.rootName + "."
                      val fieldPath =   if (d.name.startsWith(fieldPathPrefix)) d.name.substring(fieldPathPrefix.length) else ""
                      Some((prefixSymbol(d.rootName, prefix), fieldPath))
                    }
                    CFGStatement(ConnectableStatement(
                      PDGVertex(file, line, col, s"input_${d.name}", VertexKind.IO, false, modulePath, relatedSignal, None, Some(prefixSymbol(d.name, prefix)), true),
                      root,
                      Vector.empty,
                      prefixSymbols(Vector(d)),
                      false
                    )) 
                  }
                }.toVector
              } else {
                // Single input. This could use a better solution that uses less code repetition, but this will do.
                module match {
                  case _: BlackBox => {
                    Vector(
                      CFGStatement(ConnectableStatement(
                        PDGVertex(file, line, col, s"output_${p.name}", VertexKind.IO, false, modulePath, isChiselStatement = true),
                        root,
                        prefixSymbols(Vector(RegularDependency(p.name, p.name, false))),
                        Vector.empty,
                        false
                      )))
                  }
                  case _: Component => {
                    val relatedSignal = Some((prefixSymbol(p.name, prefix), ""))
                    Vector(CFGStatement(ConnectableStatement(
                          PDGVertex(file, line, col, s"input_${p.name}", VertexKind.IO, false, modulePath, relatedSignal, None, Some(prefixSymbol(p.name, prefix)), true),
                          root,
                          Vector.empty,
                          prefixSymbols(Vector(RegularDependency(p.name, "", false))),
                          false
                        )))
                  }
                }
              }
            }
          }
          case spinal.core.out => {
            //if (addCompoundSignal(prefixSymbol(p.name, prefix), p.tpe, prefix, true, typeAliases)) {
            if (true) {
              val compoundDefs = getSingleDeps(p, Seq.empty, typeAliases, Some(p.name), p.name + ".")
              compoundDefs.map{d => 
                  if (d.flipped) {
                    // It's an input node
                    val relatedSignal = {
                      val fieldPathPrefix = d.rootName + "."
                      val fieldPath =   if (d.name.startsWith(fieldPathPrefix)) d.name.substring(fieldPathPrefix.length) else ""
                      Some((prefixSymbol(d.rootName, prefix), fieldPath))
                    }
                    CFGStatement(ConnectableStatement(
                      PDGVertex(file, line, col, s"input_${d.name}", VertexKind.IO, false, modulePath, relatedSignal, None, Some(prefixSymbol(d.name, prefix)), true),
                      root,
                      Vector.empty,
                      prefixSymbols(Vector(d)),
                      false
                    )) 
                  } else {
                    // Output node
                    CFGStatement(ConnectableStatement(
                      PDGVertex(file, line, col, s"output_${d.name}", VertexKind.IO, false, modulePath, isChiselStatement = true),
                      root,
                      prefixSymbols(Vector(d)),
                      Vector.empty,
                      false
                    ))
                  }
              }.toVector
            } else {
              // It's a single output
                module match {
                  case _: BlackBox => {
                    val relatedSignal = Some((prefixSymbol(p.name, prefix), ""))
                    Vector(CFGStatement(ConnectableStatement(
                          PDGVertex(file, line, col, s"output_${p.name}", VertexKind.IO, false, modulePath, relatedSignal, None, Some(prefixSymbol(p.name, prefix)), true),
                          root,
                          Vector.empty,
                          prefixSymbols(Vector(RegularDependency(p.name, "", false))),
                          false
                        )))
                  }
                  case _: Component => {
                    Vector(
                      CFGStatement(ConnectableStatement(
                        PDGVertex(file, line, col, s"output_${p.name}", VertexKind.IO, false, modulePath, isChiselStatement = true),
                        root,
                        prefixSymbols(Vector(RegularDependency(p.name, p.name, false))),
                        Vector.empty,
                        false
                      )))
                  }
                }
            }

          }
        }
      }.toVector*/

    // Now, we make a list of all the statements that are present in the module.
    // A statement is one of the following: Definition, Connection, ControlFlow
    // For example, nodes are definitions, as are wire declarations. However, wires need to be connected (no undriven signals).
    
    // Handle external modules
    module match {
      case module: Component => {
        module.dslBody match {
          case b: ScopeStatement => {
//            val statements = extractStatements(b, root, None, getClockedElements(b), moduleMap, typeAliases, prefix, moduleInstanceDependency = moduleDep)
            val statements = scopeToCFG(b, root, None)

            // Combine all the statements
            if (isMain) {
              ioStatements ++ statements
            } else {
              // Make all I/O definitions instead (for complete slicing only).
              val ioDefs = ioStatements.map {node =>
                val newVert = node.stmt.vertex.copy(kind = Definition)
                node.copy(stmt = node.stmt.copy(vertex = newVert))
              }
              statements ++ ioDefs
            }
          }
          case _: Statement => Vector.empty
        }
      }
      // In the special case of external modules, we need to provide connection nodes, because the blackbox will not provide
      // any dependencies.
      case module: BlackBox => {
        ioStatements.map(stmt => {
          val newVert = stmt.stmt.vertex.copy(kind = Connection)
          stmt.copy(stmt.stmt.copy(vertex=newVert))
        })
      }
      case _ => Vector.empty
    }
  }

  def matchDependencies(stmts: Seq[CFGNode]): (Vector[PDGVertex], Vector[PDGEdge]) = {
    // This makes a map that lists all nodes that provide a certain statement
    def getConnectableStatements(s: Seq[CFGNode]): Seq[ConnectableStatement] = {
      s.flatMap {
        case CFGStatement(stmt) => Seq(stmt)
        case CFGFork(stmt, _, _, left, right) => Seq(stmt) ++ getConnectableStatements(left) ++ getConnectableStatements(right)
      }
    }

    val allStatements = getConnectableStatements(stmts)

    // Todo: Check out the memory statements
    val flattenedStmts: Seq[ConnectableStatement] = allStatements

    val providerMap = flattenedStmts.flatMap(stmt => stmt.provides.map(provider => provider.name -> stmt))
      .groupBy(_._1)
      .map { case (provider, pairs) => provider -> pairs.map(_._2) }

    // Now that there is a provider map, the dependencies can be matched and edges can be created.
    val (allVerts, allEdges) = flattenedStmts.foldLeft((Vector.empty[PDGVertex], Vector.empty[PDGEdge])) {
      case ((verts, edges), s: ConnectableStatement) => {
        val deps: Seq[(ConnectableStatement, PDGDependency)] = s.dependencies.flatMap(d => providerMap.getOrElse(d.name, {
          println(s"Warning: dependency without provider: ${d.name}; Vertex:")
          println(s)
          Seq.empty[ConnectableStatement]
        }).map(x => (x, d)))
        val newEdges = deps.flatMap { d =>
          val edgeCondition = d._2 match {
            case r: RegularDependency => None
            case ConditionalDependency(_, _, _, _, conditionSignals, conditionValues) => Some(PDGCondition(conditionSignals, conditionValues))
          }

          val isIndexEdge = d._2 match {
            case r: RegularDependency => r.isIndex
            case c: ConditionalDependency => false
          }

          if (isIndexEdge) {
            Seq(PDGEdge(s.vertex, d._1.vertex, EdgeKind.Index, s.clocked, edgeCondition))
          } else {
            d._1.vertex.kind match {
              case DataDefinition => Seq(
                PDGEdge(s.vertex, d._1.vertex, EdgeKind.Data, s.clocked, edgeCondition),
                PDGEdge(s.vertex, d._1.vertex, EdgeKind.Declaration, false, edgeCondition)
              )
              case Connection => Seq(PDGEdge(s.vertex, d._1.vertex, EdgeKind.Data, s.clocked, edgeCondition))
              case ControlFlow => Seq(PDGEdge(s.vertex, d._1.vertex, EdgeKind.Conditional, s.clocked, edgeCondition))
              case Definition => Seq(PDGEdge(s.vertex, d._1.vertex, EdgeKind.Declaration, false, edgeCondition))
              case IO => Seq(PDGEdge(s.vertex, d._1.vertex, EdgeKind.Data, s.clocked, edgeCondition))
            }
          }
        }

        (verts :+ s.vertex, edges ++ newEdges)
      }
    }

    (allVerts, allEdges)
  }

  /// Recursively gets individual dependencies from a compound datatype signal.
  def getSingleDeps(tpe: Data, path: Seq[CompoundSignalPathNode], typeAliases: Seq[DefTypeAlias], rootSymbol: Option[String] = None, prefix: String = ""): Vector[PDGDependency] = {
    // When "path" runs out, it means that from that point onwards, all dependencies that are left in the reduced tree should be returned
    val (pathHead, pathTail): (Option[CompoundSignalPathNode], Seq[CompoundSignalPathNode]) = path match {
      case head +: tail => (Some(head), tail)
      case _ => (None, Seq.empty)
    }

    // All compound signals are `MultiData`
    tpe match {
      /*case vec: Vec[?] => {
        val nextDeps = getSingleDeps(vec.dataType, pathTail, typeAliases, rootSymbol)
      }
      case bundle: Bundle => {
        val fieldMap = bundle.elements.map(e => (e.name, e.dataType)).toMap
        val nextDeps = fieldMap.flatMap{ case (fieldName, fieldType) =>
          val newPathNode = BundleField(fieldName)
          getSingleDeps(fieldType, pathTail, typeAliases, rootSymbol, prefix + "." + fieldName)
        }.toVector
      }*/
      case _ => Vector.empty
    }

    Vector.empty // TODO: implement this method
  }

  def scopeToCFG(root: ScopeStatement, sourceModule: String, condition: Option[PDGDependency]): Vector[CFGNode] = {
    var cfgNodes: Vector[CFGNode] = Vector.empty
    val conditionalDep = condition.toVector
    root.foreachStatements {
      case assignment: AssignmentStatement => {
        println(s"Assignment expression: ${assignment.target} = ${assignment.source} ${assignment.locationString}")
        val (file, line, col) = getSourceLocation(assignment)
        val sourceSymbols = expressionToSymbols(assignment.source)
        val targetSymbols = expressionToSymbols(assignment.target)
        val target = assignment.target
        // We must know whether the target is a register or not, to let the slicer know that this dependency is spread over time.
        val isReg = target match {
          case t: BaseType => t.isReg
          case _ => false
        }
        val sourceName = sourceSymbols.head.name
        val targetName = targetSymbols.head.name
        val relatedSignal = Some((targetName, ""))
        val cfg = CFGStatement(
          ConnectableStatement(
            PDGVertex(file, line, col, targetName, VertexKind.Connection, isReg, Seq(), relatedSignal, assignsTo = Some(targetName)),
            sourceModule,
            dependencies = sourceSymbols ++ conditionalDep,
            provides = targetSymbols,
            clocked = isReg
          )
        )
        cfgNodes :+= cfg
      }
      case conditional: TreeStatement => {
        val predExpr = conditional match {
          case whenStmt: WhenStatement => {
            println(s"Condition: ${whenStmt.cond}, ${whenStmt.whenTrue}, ${whenStmt.whenFalse}")
            whenStmt.cond
          }
          case switchStmt: SwitchStatement => {
            println(s"Switch value: ${switchStmt.value}, cases: ${switchStmt.elements.map(c => (c.keys, c.scopeStatement))}, default: ${switchStmt.defaultScope}")
            switchStmt.value
          }
        }
        val clocked = false
        val condVertexName = predExpr match {
          case s: BaseType => s.name
          case _ => generateRandomString(10)
        }
        val nodeName = s"cond $condVertexName"

        val (left, right) = conditional match {
          case whenStmt: WhenStatement => (whenStmt.whenTrue, whenStmt.whenFalse)
        }
        val nestedConditionalDependency = RegularDependency(nodeName, nodeName, flipped = false)
        val leftCFG = scopeToCFG(left, sourceModule, Some(nestedConditionalDependency))
        val rightCFG = scopeToCFG(right, sourceModule, Some(nestedConditionalDependency))
        val (file, line, col) = getSourceLocation(conditional)
        val relatedSignal = Some((condVertexName, ""))
        val predDependency = RegularDependency(condVertexName, condVertexName, flipped = false)
        val cfg = CFGFork(
          ConnectableStatement(
            PDGVertex(file, line, col, nodeName, VertexKind.ControlFlow, clocked, Seq(), relatedSignal, condition=None),
            sourceModule,
            dependencies = conditionalDep ++ Vector(predDependency),
            provides = Vector(nestedConditionalDependency),
            clocked = clocked
          ),
          condVertexName,
          "",
          leftCFG,
          rightCFG,
        )
        cfgNodes :+= cfg
      }
      case baseType: BaseType => {
        val clocked = baseType.isReg
        // Todo: What about inout ports?
        val flipped = baseType.isInput
        val isIO = !baseType.isDirectionLess
        val kind = if (isIO) VertexKind.IO else VertexKind.Definition
        val nodeName = if (isIO) s"IO ${baseType.name}" else s"def ${baseType.name}"
        val dependency = RegularDependency(baseType.name, "", flipped = flipped)
        val (file, line, col) = getSourceLocation(baseType)
        val relatedSignal = Some((baseType.name, ""))
        val isProvider = !isIO || flipped
        val cfg = CFGStatement(
          ConnectableStatement(
            PDGVertex(file, line, col, nodeName, kind, clocked, Seq(), relatedSignal, assignsTo = Some(baseType.name)),
            sourceModule,
            dependencies = if (!isProvider) Seq(dependency) else Seq(),
            provides = if (isProvider) Seq(dependency) else Seq(),
            clocked = clocked
          )
        )
        cfgNodes :+= cfg
      }
      case stmt =>
        println(s"Statement ${stmt.getClassIdentifier} $stmt not implemented")
    }
    cfgNodes
  }

  def expressionToSymbols(expr: Expression, depth: Int = 1): Vector[PDGDependency] = {
    val indent = " " * depth
    val symbols: Vector[PDGDependency] = expr match {
      case a: BinaryOperator => expressionToSymbols(a.left, depth+1) ++ expressionToSymbols(a.right, depth+1)
      case a: UnaryOperator => expressionToSymbols(a.source)
      case l: Literal => Vector.empty
      case x: SubAccess => {
        println(s"${indent}SubAccess: ${x.getClass}, value: $x")
        Vector.empty
      }
      // This indicates a reference to some signal
      case b: BaseType => {
        println(s"${indent}BaseType: ${b.getClass}, value: $b")
        Vector(RegularDependency(b.name, "", flipped = false))
      }
      case _ => {
        println(s"${indent}Expression type ${expr.getClass}: $expr.")
        Vector.empty
      }
    }
    symbols
  }

  def extractStatements(root: ScopeStatement, sourceModule: String,
    condition: Option[PDGDependency],
    clockedElements: Set[String],
    moduleMap: Map[String, Component],
    typeAliases: Seq[DefTypeAlias],
    prefix: String = "",
    seqMemsTop: Vector[String] = Vector.empty, // This is used to track the sequential memories
    moduleInstanceDependency: Option[PDGDependency] = None // If the the ScopeStatement is a module instance, use this to attach the module definition statement as dependency
  ): Vector[CFGNode] = {
    val seqMems: ArrayBuffer[String] = ArrayBuffer(seqMemsTop: _*)
    val modulePath = getModulePath(prefix)

  root.foreachStatements(stmt => {
      val (file, parsedLine, parsedCol) = getSourceLocation(stmt)
      val cond_dep = condition.toVector
      val moduleInstDep = moduleInstanceDependency.toVector

      stmt match {
        case assignment: AssignmentStatement => {
          // Handle connections and definitions. This includes nodes, wires, registers, and connections between them.
          // For each of these statements, we need to determine the dependencies and the provided symbols, as well as the type of the statement (definition, connection, control flow)
          // We also need to determine if the statement is clocked or not, which is true if it contains any register definitions or connections to registers.
          // For connections, we also need to determine the direction of the connection (flipped or not), which is determined by the direction of the signal in the module definition and whether the connection is on the left-hand side or right-hand side of the connection.

          val lhsSymbols = extractSymbolsOriginal(assignment.target, prefix, typeAliases)
          val rhsSymbols = extractSymbolsOriginal(assignment.source, prefix, typeAliases)
          println(s"LHS symbols: $lhsSymbols")
          println(s"RHS symbols: $rhsSymbols")
          val clocked = lhsSymbols.exists(s => clockedElements.contains(s.rootName))
        }
        case conditional: TreeStatement => {
          val predExpr = conditional match {
            case whenStmt: WhenStatement => {
              println(s"Condition: ${whenStmt.cond}, ${whenStmt.whenTrue}, ${whenStmt.whenFalse}")
              whenStmt.cond
            }
            case switchStmt: SwitchStatement => {
              println(s"Switch value: ${switchStmt.value}, cases: ${switchStmt.elements.map(c => (c.keys, c.scopeStatement))}, default: ${switchStmt.defaultScope}")
              switchStmt.value
            }
          }
          val condVertexName = predExpr match {
            case s: BaseType => s.name
            case _ => generateRandomString(10)
          }
          /*val pred_stmt = ConnectableStatement(
            PDGVertex(file, parsedLine, parsedCol, condVertexName, VertexKind.ControlFlow, false, modulePath, relatedSignal, isChiselStatement=true),
            sourceModule,
            prefixSymbols(predSymbols ++ indexDeps ++ cond_dep ++ moduleInstDep),
            prefixSymbols(condVertexDep.toVector),
            false
          )
          Vector(CFGFork(predExpr, c.pred.serialize, prefix, conseq_stmts, alt_stmts))*/
        }
        case _ => {
          println(s"Statement type ${stmt.getClass}: $stmt.")
        }
      }
    })
    Vector()
  }

  def extractSymbolsOriginal(expr: Expression, prefix: String, typeAliases: Seq[DefTypeAlias], ignore: Set[String] = Set.empty, findAllDeps: Boolean = true): Vector[PDGDependency] = {
    val symbols: Vector[PDGDependency] = expr match {
      case l: Literal => Vector.empty
      case x: SubAccess => {
        println(s"SubAccess: ${x.getClass}, value: $x")
        Vector.empty
      }
      case b: BaseType => {
        println(s"BaseType: ${b.getClass}, value: $b")
        Vector(RegularDependency(b.name, b.name, false))
      }
      case _ => {
        println(s"Expression type ${expr.getClass}: $expr.")
        Vector.empty
      }
    }

    symbols.filter(d => !ignore.contains(d.name))
  }

  def getClockedElements(root: ScopeStatement): Set[String] = {
    val set: Set[String] = Set.empty
    root.walkLeafStatements((stmt => {
      stmt match {
        // TODO ADAPT2SPINAL
        /*case r: DefRegister => set += r.name
        case r: DefRegisterWithReset => set += r.name
        case c: Conditionally => {
          val conseq_stmts = c.conseq match {
            case b: Block => getClockedElements(b)
            case _ => Set.empty
          }
          val alt_stmts = c.alt match {
            case b: Block => getClockedElements(b)
            case _ => Set.empty
          }

          set ++= conseq_stmts ++ alt_stmts
        }*/
        case s => // Do nothing
      }
    }))
    set
  }
}
