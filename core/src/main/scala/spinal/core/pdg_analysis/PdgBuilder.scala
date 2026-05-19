package spinal.core.pdg_analysis

import scala.util.Random
import scala.collection.mutable.{HashMap, ArrayBuffer, Set}
import spinal.core.Component
import spinal.core.internals._
import spinal.core.BlackBox
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
    ("unknown_file", 0, 0) // TODO: implement this properly. This is a placeholder to allow for testing of the rest of the code without having to worry about file info parsing.
  }

  def prefixSymbol(symbol: String, prefix: String): String = {
    if (prefix.isBlank()) {
      symbol
    } else {
      prefix + "." + symbol
    }
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

  case class ConnectableStatement(
    vertex: PDGVertex,
    sourceModule: String, // The module that the statement is from
    dependencies: Seq[PDGDependency],
    provides: Seq[PDGDependency], // The symbols that the statement provides. This is used to in conjunction with the dependencies to form edges.
    clocked: Boolean,
    isInferredMemoryDirection: Option[InferDirection] = None
  )

  sealed trait CFGNode

  // The implicit assumption is that the graph is stored in a sequence and that sequential elements are connected.
  // This vastly simplifies the datastructure that is needed to store the graph and allows for easier graph generation.
  case class CFGStatement(
    stmt: ConnectableStatement
  ) extends CFGNode

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
    val moduleMap = modules.map(m => (m.name, m)).toMap
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

    val ioStatements =
      module.ioSet.flatMap {p =>
        val (file, line, col) = getSourceLocation(p)
        // Todo: SPINAL check inout and other special types
        p.dir match {
          case in => {
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
                val compoundDefs = getSingleDeps(new TPE(), Seq.empty, typeAliases, Some(p.name), p.name + ".")
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
                }
              }
            }
          }
          case out => {
            //if (addCompoundSignal(prefixSymbol(p.name, prefix), p.tpe, prefix, true, typeAliases)) {
            if (true) {
              val compoundDefs = getSingleDeps(new TPE(), Seq.empty, typeAliases, Some(p.name), p.name + ".")
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
                }
            }

          }
        }
      }.toVector

    // Now, we make a list of all the statements that are present in the module.
    // A statement is one of the following: Definition, Connection, ControlFlow
    // For example, nodes are definitions, as are wire declarations. However, wires need to be connected (no undriven signals).
    
    // Handle external modules
    module match {
      case module: Component => {
        module.dslBody match {
          case b: ScopeStatement => {
            val statements = extractStatements(b, root, None, getClockedElements(b), moduleMap, typeAliases, prefix, moduleInstanceDependency = moduleDep)

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
    (Vector.empty, Vector.empty) // TODO: implement this method to match the dependencies and generate the vertices and edges of the PDG.
  }

  case class TPE()

  def getSingleDeps(tpe: TPE, path: Seq[CompoundSignalPathNode], typeAliases: Seq[DefTypeAlias], rootSymbol: Option[String] = None, prefix: String = ""): Vector[PDGDependency] = {
    Vector.empty // TODO: implement this method
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
    Vector()
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
