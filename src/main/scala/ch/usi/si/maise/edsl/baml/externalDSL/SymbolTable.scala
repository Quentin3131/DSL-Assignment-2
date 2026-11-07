package ch.usi.si.maise.edsl.baml.externalDSL

import ch.usi.si.maise.edsl.baml.model.*
import scala.util.Try

/** A symbol table for the BAML DSL.
  *
  * All declarations (classes, enums, clients, and functions) share the same
  * namespace.
  *
  * Types must be declared before being used: a class or enum is added to the
  * symbol table only once its declaration is complete, so it cannot refer to
  * itself or to types declared after it.
  *
  * Not all methods are needed to implement the requested (especially non-bonus)
  * features.
  *
  * @param nameToDeclaration
  *   a map from name to declarations.
  * @param currentDeclaration
  *   the class or enum being declared - to be used only in Exercise 4.1
  * @param currentMembers
  *   the members of the class or enum being declared, in order - to be used
  *   only in Exercise 4.1
  */
private[externalDSL] case class SymbolTable(
    private val nameToDeclaration: Map[Name, BAMLDeclaration] = Map(),
    private val currentDeclaration: Option[CurrentDeclaration] = None,
    private val currentMembers: Seq[BAMLMember] = Seq()
):

  /** Adds a declaration (class, enum, client, or function) to the symbol table.
    */
  def addDeclaration(
      declaration: BAMLDeclaration
  ): Try[SymbolTable] =
    Try:
      val name = declaration.name
      if (nameToDeclaration.contains(name)) then
        throw IllegalArgumentException(
          s"name ${name.rep} has been already declared as ${nameToDeclaration(name)}"
        )
      this.copy(
        nameToDeclaration = nameToDeclaration + (name -> declaration)
      )

  /** Resolves a name as a class or fails if the name is undefined or has been
    * declared as something which is not a class. It returns a class reference,
    * to be used as a type.
    *
    * @param name
    *   the name of a previously declared class.
    */
  def resolveClass(name: Name): Try[BAMLClassReference] =
    Try:
      if (!nameToDeclaration.contains(name))
        throw IllegalArgumentException(
          s"class named ${name.rep} has not been declared"
        )
      nameToDeclaration(name) match
        case c: BAMLClassDeclaration => BAMLClassReference(c.name)
        case _ =>
          throw IllegalArgumentException(
            s"Name ${name.rep} has not been declared as a class"
          )

  /** Resolves a name as an enum or fails if the name is undefined or has been
    * declared as something which is not an enum. It returns an enum reference,
    * to be used as a type.
    *
    * @param name
    *   the name of a previously declared enum.
    */
  def resolveEnum(name: Name): Try[BAMLEnumReference] =
    Try:
      if (!nameToDeclaration.contains(name))
        throw IllegalArgumentException(
          s"enum named ${name.rep} has not been declared"
        )
      nameToDeclaration(name) match
        case e: BAMLEnumDeclaration => BAMLEnumReference(e.name)
        case _ =>
          throw IllegalArgumentException(
            s"Name ${name.rep} has not been declared as an enum"
          )

  /** Resolves a name as a declared type, returning a class reference or an enum
    * reference depending on how the name has been declared.
    *
    * @param name
    *   the name of a previously declared class or enum.
    */
  def resolveType(name: Name): Try[BAMLTypeReference] =
    Try:
      if (!nameToDeclaration.contains(name))
        throw IllegalArgumentException(
          s"Type named ${name.rep} has not been declared"
        )
      nameToDeclaration(name) match
        case c: BAMLClassDeclaration => BAMLClassReference(c.name)
        case e: BAMLEnumDeclaration  => BAMLEnumReference(e.name)
        case _ =>
          throw IllegalArgumentException(
            s"Name ${name.rep} has not been declared as a type"
          )

  /** Resolves a name as a client or fails if the name is undefined or has been
    * declared as something which is not a client. It returns a client
    * reference, to be used in functions.
    *
    * @param name
    *   the name of a previously declared client.
    */
  def resolveClient(name: Name): Try[BAMLClientReference] =
    Try:
      if (!nameToDeclaration.contains(name))
        throw IllegalArgumentException(
          s"client named ${name.rep} has not been declared"
        )
      nameToDeclaration(name) match
        case c: BAMLClientDeclaration => BAMLClientReference(c.name)
        case _ =>
          throw IllegalArgumentException(
            s"Name ${name.rep} has not been declared as a client"
          )

  // EXERCISES 4 ONLY

  /** Enters the declaration of a class with the given name.
    *
    * @param name
    *   the name of the class being declared.
    * @return
    *   the modified symbol table, or a failure if the name has been already
    *   declared.
    */
  def enterCurrentClass(name: Name): Try[SymbolTable] =
    enterCurrentDeclaration(CurrentDeclaration.CurrentClass(name))

  /** Enters the declaration of an enum with the given name.
    *
    * @param name
    *   the name of the enum being declared.
    * @return
    *   the modified symbol table, or a failure if the name has been already
    *   declared.
    */
  def enterCurrentEnum(name: Name): Try[SymbolTable] =
    enterCurrentDeclaration(CurrentDeclaration.CurrentEnum(name))

  private def enterCurrentDeclaration(
      declaration: CurrentDeclaration
  ): Try[SymbolTable] =
    Try:
      val name = declaration.name
      if (nameToDeclaration.contains(name)) then
        throw IllegalArgumentException(
          s"name ${name.rep} has been already declared as ${nameToDeclaration(name)}"
        )
      this.copy(
        currentDeclaration = Some(declaration),
        currentMembers = Seq()
      )

  /** Adds a member to the class or enum being declared.
    *
    * This methods ONLY modifies the symbol table, not the declaration - the
    * declaration will be constructed only by calling the
    * exitCurrentDeclaration() method.
    *
    * It checks that:
    *   - fields are added to classes, and enum values to enums;
    *   - no member with the same name has been already added.
    *
    * @param member
    *   the member to add.
    * @return
    *   the modified symbol table, or a failure.
    */
  def addMemberToCurrentDeclaration(member: BAMLMember): Try[SymbolTable] =
    Try:
      val declaration = this.currentDeclaration match
        case Some(declaration) => declaration
        case None =>
          throw IllegalStateException(
            "No current declaration, cannot add member."
          )

      (declaration, member) match
        case (CurrentDeclaration.CurrentClass(_), _: BAMLClassField) => ()
        case (CurrentDeclaration.CurrentEnum(_), _: BAMLEnumValue)   => ()
        case _ =>
          throw IllegalArgumentException(
            s"Cannot add member ${member.name.rep} to ${declaration.name.rep}: fields belong to classes, values to enums."
          )

      if (currentMembers.exists { _.name == member.name }) then
        throw IllegalArgumentException(
          s"member ${member.name.rep} has been already declared in ${declaration.name.rep}"
        )
      this.copy(currentMembers = currentMembers :+ member)

  /** Exits the declaration of the current class or enum, returning it complete
    * with all its members, in the order in which they have been added.
    *
    * @return
    *   the class or enum declaration, or a failure if there is no current
    *   declaration, or if the declaration is not valid (e.g., an enum without
    *   values).
    */
  def exitCurrentDeclaration(): Try[BAMLTypeDeclaration] =
    Try:
      this.currentDeclaration match
        case Some(CurrentDeclaration.CurrentClass(name)) =>
          BAMLClassDeclaration(
            name,
            currentMembers.collect:
              case field: BAMLClassField => field
          )
        case Some(CurrentDeclaration.CurrentEnum(name)) =>
          BAMLEnumDeclaration(
            name,
            currentMembers.collect:
              case value: BAMLEnumValue => value
          )
        case None =>
          throw IllegalStateException(
            "Cannot exit before entering a declaration."
          )

/** The class or enum currently being declared - to be used only in Exercise
  * 4.1.
  */
private[externalDSL] enum CurrentDeclaration(val name: Name):
  case CurrentClass(className: Name) extends CurrentDeclaration(className)
  case CurrentEnum(enumName: Name) extends CurrentDeclaration(enumName)
