package ch.usi.si.maise.edsl.baml.model

/** A shared trait for all the BAML elements.
  */
sealed trait BAMLElement:
  /** This method converts the solution domain object back into a possible
    * BAML DSL script.
    *
    * You should never use this method, or touch the implementation of it for
    * the existing classes. If you suspect there is a bug, please let the
    * instructor know.
    */
  def rep: String

/** Represents a name (identifier) in a BAML file.
  * @param value
  *   the string value for the name. Must not be empty.
  */
case class Name(val rep: String):
  require(rep.length > 0)

/** A BAML file, expressed as a sequence of top level elements (i.e., class,
  * enum, client, or function declarations.)
  *
  * @param elements
  *   the elements in the file.
  */
case class BAMLFile(elements: Seq[BAMLTopLevelElement]) extends BAMLElement:
  def rep = elements.map { _.rep }.mkString("\n\n")

//** Values

/** A value, used in attributes and in client options. It can be a literal or
  * a reference to an environment variable.
  */
sealed trait BAMLValue extends BAMLElement

/** A String value.
  *
  * @param value
  */
case class BAMLStringValue(val value: String) extends BAMLValue:
  def rep = s"\"${value}\""

/** An integer value.
  */
case class BAMLIntValue(val value: Int) extends BAMLValue:
  def rep = value.toString()

/** A floating point value.
  */
case class BAMLFloatValue(val value: Double) extends BAMLValue:
  def rep = value.toString()

/** A boolean value.
  */
case class BAMLBoolValue(val value: Boolean) extends BAMLValue:
  def rep = value.toString()

/** A reference to an environment variable, i.e. `env.NAME`.
  *
  * @param name
  *   the name of the environment variable.
  */
case class BAMLEnvVariable(name: Name) extends BAMLValue:
  def rep = s"env.${name.rep}"

//** Declarations

/** A top level element in the file. For now, only declarations.
  */
sealed trait BAMLTopLevelElement extends BAMLElement

/** A named declaration (type, client, or function). All declarations share the
  * same namespace.
  */
sealed trait BAMLDeclaration extends BAMLTopLevelElement:
  val name: Name

/** A type declaration (class or enum).
  */
sealed trait BAMLTypeDeclaration extends BAMLDeclaration

/** A class, which is composed of fields.
  *
  * @param name
  *   the name (identifier) of the class.
  * @param fields
  *   a sequence of fields.
  */
case class BAMLClassDeclaration(
    name: Name,
    fields: Seq[BAMLClassField] = Seq()
) extends BAMLTypeDeclaration:
  def rep =
    s"class ${name.rep} ${fields.map { _.rep }.mkString("{\n  ", "\n  ", "\n}")}"

  def testRep = this.rep.replace("\n", "\\n")

/** An enum, which is composed of values.
  *
  * @param name
  *   the name (identifier) of the enum.
  * @param values
  *   a sequence of enum values. Must not be empty.
  */
case class BAMLEnumDeclaration(
    name: Name,
    values: Seq[BAMLEnumValue]
) extends BAMLTypeDeclaration:
  require(values.nonEmpty, "an enum must have at least one value")

  def rep =
    s"enum ${name.rep} ${values.map { _.rep }.mkString("{\n  ", "\n  ", "\n}")}"

  def testRep = this.rep.replace("\n", "\\n")

/** A member of a type declaration.
  *
  * Classes are composed of fields, enums are composed of values. Both can have
  * an optional alias and an optional description.
  */
sealed trait BAMLMember extends BAMLElement:
  val name: Name
  val optionalAlias: Option[BAMLStringValue]
  val optionalDescription: Option[BAMLStringValue]

  /** The representation of the attributes of the member, if any.
    */
  def attributesRep =
    optionalAlias.map { a => s" @alias(${a.rep})" }.getOrElse("") +
      optionalDescription.map { d => s" @description(${d.rep})" }.getOrElse("")

/** A class field.
  *
  * @param name
  *   the name of the field.
  * @param fieldType
  *   the type of the field.
  * @param optionalAlias
  *   the alias of the field, i.e. the name shown to the LLM.
  * @param optionalDescription
  *   the description of the field.
  */
case class BAMLClassField(
    name: Name,
    fieldType: BAMLType,
    optionalAlias: Option[BAMLStringValue] = None,
    optionalDescription: Option[BAMLStringValue] = None
) extends BAMLMember:
  def rep = s"${name.rep} ${fieldType.rep}${attributesRep}"

/** An enum value.
  *
  * @param name
  *   the name of the value.
  * @param optionalAlias
  *   the alias of the value, i.e. the name shown to the LLM.
  * @param optionalDescription
  *   the description of the value.
  */
case class BAMLEnumValue(
    name: Name,
    optionalAlias: Option[BAMLStringValue] = None,
    optionalDescription: Option[BAMLStringValue] = None
) extends BAMLMember:
  def rep = s"${name.rep}${attributesRep}"

//** TYPES

/** A type, used for class fields.
  */
sealed trait BAMLType extends BAMLElement

/** Primitive type.
  *
  * Can be bool, int, float, string, or null.
  */
enum BAMLPrimitiveType(val rep: String) extends BAMLType:
  case BoolType extends BAMLPrimitiveType("bool")
  case IntType extends BAMLPrimitiveType("int")
  case FloatType extends BAMLPrimitiveType("float")
  case StringType extends BAMLPrimitiveType("string")
  case NullType extends BAMLPrimitiveType("null")

/** A reference to a declared type (class or enum).
  */
sealed trait BAMLTypeReference extends BAMLType:
  val name: Name
  def rep = name.rep

/** A reference to a declared class.
  */
case class BAMLClassReference(name: Name) extends BAMLTypeReference

/** A reference to a declared enum.
  */
case class BAMLEnumReference(name: Name) extends BAMLTypeReference

/** An optional type, i.e. `T?`.
  *
  * @param innerType
  *   the type which is made optional. Cannot be a list.
  */
case class BAMLOptionalType(innerType: BAMLType) extends BAMLType:
  require(
    innerType match
      case _: BAMLListType => false
      case _               => true
    ,
    "list types cannot be optional"
  )

  def rep = s"${innerType.rep}?"

/** A list type, i.e. `T[]`.
  *
  * @param elementType
  *   the type of the elements of the list.
  */
case class BAMLListType(elementType: BAMLType) extends BAMLType:
  def rep = s"${elementType.rep}[]"

/** A map type, i.e. `map<K, V>` (Exercise 4.2).
  *
  * @param keyType
  *   the type of the keys. Must be string or an enum.
  * @param valueType
  *   the type of the values.
  */
case class BAMLMapType(keyType: BAMLType, valueType: BAMLType)
    extends BAMLType:
  require(
    keyType match
      case BAMLPrimitiveType.StringType => true
      case _: BAMLEnumReference         => true
      case _                            => false
    ,
    "map keys must be string or an enum"
  )

  def rep = s"map<${keyType.rep}, ${valueType.rep}>"

//** CLIENTS

/** An LLM client, i.e. `client<llm> Name { provider ... options { ... } }`.
  *
  * @param name
  *   the name (identifier) of the client.
  * @param provider
  *   the provider of the LLM.
  * @param options
  *   the options passed to the provider. Must include a `model` option with a
  *   string value.
  */
case class BAMLClientDeclaration(
    name: Name,
    provider: BAMLProvider,
    options: Seq[BAMLClientOption]
) extends BAMLDeclaration:
  require(
    options.exists { option =>
      option match
        case BAMLClientOption(Name("model"), _: BAMLStringValue) => true
        case _                                                  => false
    },
    "a client must have a model option with a string value"
  )

  def rep =
    s"client<llm> ${name.rep} {\n  provider ${provider.rep}\n  options ${options
        .map { _.rep }
        .mkString("{\n    ", "\n    ", "\n  }")}\n}"

  def testRep = this.rep.replace("\n", "\\n")

/** An LLM provider.
  */
enum BAMLProvider(val rep: String) extends BAMLElement:
  case OpenAI extends BAMLProvider("openai")
  case OpenAIResponses extends BAMLProvider("openai-responses")
  case OpenAIGeneric extends BAMLProvider("openai-generic")
  case AzureOpenAI extends BAMLProvider("azure-openai")
  case Anthropic extends BAMLProvider("anthropic")
  case GoogleAI extends BAMLProvider("google-ai")
  case VertexAI extends BAMLProvider("vertex-ai")
  case AWSBedrock extends BAMLProvider("aws-bedrock")

/** A client option, i.e. a key/value pair.
  *
  * @param key
  *   the name of the option.
  * @param value
  *   the value of the option.
  */
case class BAMLClientOption(key: Name, value: BAMLValue) extends BAMLElement:
  def rep = s"${key.rep} ${value.rep}"

/** A reference to a declared client.
  */
case class BAMLClientReference(name: Name) extends BAMLElement:
  def rep = name.rep

//** FUNCTIONS

/** An LLM function, i.e.
  * `function Name(param: Type, ...) -> Type { client ... prompt #"..."# }`.
  *
  * @param name
  *   the name (identifier) of the function. Must start with an uppercase
  *   letter.
  * @param parameters
  *   the parameters of the function. Must not be empty, and parameter names
  *   must be distinct.
  * @param returnType
  *   the type returned by the function.
  * @param client
  *   the client used to call the LLM.
  * @param prompt
  *   the prompt sent to the LLM.
  */
case class BAMLFunctionDeclaration(
    name: Name,
    parameters: Seq[BAMLParameter],
    returnType: BAMLType,
    client: BAMLClientReference,
    prompt: BAMLPrompt
) extends BAMLDeclaration:
  require(
    name.rep.head.isUpper,
    "function names must start with an uppercase letter"
  )
  require(parameters.nonEmpty, "a function must have at least one parameter")
  require(
    parameters.map { _.name }.distinct.size == parameters.size,
    "parameter names must be distinct"
  )

  def rep =
    s"function ${name.rep}(${parameters.map { _.rep }.mkString(", ")}) -> ${returnType.rep} {\n  client ${client.rep}\n  prompt ${prompt.rep}\n}"

  def testRep = this.rep.replace("\n", "\\n")

/** A function parameter.
  *
  * @param name
  *   the name of the parameter.
  * @param parameterType
  *   the type of the parameter.
  */
case class BAMLParameter(name: Name, parameterType: BAMLType)
    extends BAMLElement:
  def rep = s"${name.rep}: ${parameterType.rep}"

/** A prompt, i.e. a block string `#"..."#`. The content is kept as it is,
  * including newlines and double quotes.
  *
  * @param value
  *   the content of the prompt.
  */
case class BAMLPrompt(value: String) extends BAMLElement:
  def rep = s"#\"${value}\"#"
