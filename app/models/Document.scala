package models

import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.threadLocalSession
import db.GenericDao
import scalaz.{Failure, Success, Validation}
import util.DocumentType
import util.Sonstiges
import util.DocumentType
import util.DocumentType

/**
 * A document representation.
 *
 * @author andreas
 * @version 0.0.1, 2015-03-08
 */
case class Document(override val id: Option[Long] = None,
                    val name: String,
                    val description: Option[String],
                    val category: DocumentType = Sonstiges,
                    val url: String,
                    val encryptionKey: Option[String],
                    val fileSize: Option[Long],
                    override val created: Long = System.currentTimeMillis(),
                    override val creator: String,
                    override val modified: Option[Long] = None,
                    override val modifier: Option[String] = None) extends Entity(id, created, creator, modified, modifier) {

  require(Option(this.name).isDefined)
  require(Option(this.url).isDefined)
}

/**
 * Data access object for {@link Document}s.
 *
 * @author andreas
 * @version 0.0.1, 2015-03-08
 */
object Documents extends Table[Document]("Document") with GenericDao[Document] {

  import scala.slick.lifted.MappedTypeMapper.base

  implicit val documentTypeMapper = base[DocumentType, Int](ps => ps.id, id => Sonstiges.getDocumentType(id).get)

  override def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def name = column[String]("name")

  def description = column[String]("description", O.Nullable)

  def category = column[DocumentType]("category")

  def url = column[String]("url")

  def encryptionKey = column[String]("encryptionKey", O.Nullable)

  def fileSize = column[Long]("fileSize", O.Nullable)

  def created = column[Long]("created")

  def creator = column[String]("creator")

  def modified = column[Long]("modified", O.Nullable)

  def modifier = column[String]("modifier", O.Nullable)

  def * = id.? ~ name ~ description.? ~ category ~ url ~ encryptionKey.? ~ fileSize.? ~ created ~ creator ~ modified.? ~ modifier.? <>(Document.apply _, Document.unapply _)

  def withoutId = name ~ description.? ~ category ~ url ~ encryptionKey.? ~ fileSize.? ~ created ~ creator ~ modified.? ~ modifier.? returning id

  /**
   * Insert a new <em>doc</em> into the database.
   */
  private def insert = db withSession { (doc: Document) => withoutId.insert(doc.name, doc.description, doc.category, doc.url, doc.encryptionKey, doc.fileSize, doc.created, doc.creator, doc.modified, doc.modifier) }

  /**
   * {@inheritDoc}
   */
  override def update(doc: Document): Int = db withSession {
    Documents.where(_.id === doc.id).update(doc.copy(modified = Some(System.currentTimeMillis())))
  }

  /**
   * Save or update the specified <em>doc</em>.
   *
   * @param doc The document to persist or update in the database.
   */
  def saveOrUpdate(doc: Document): Validation[Throwable, Document] = db withSession {
    if (doc.id.isDefined) {
      val docUpd = doc.copy(modified = Some(System.currentTimeMillis()))
      try {
        val upd = this.update(docUpd)
        if (upd > 0) {
          Success(docUpd)
        } else {
          Failure(new RuntimeException("Failed to update document " + upd))
        }
      } catch {
        case e: Throwable => Failure(e)
      }
    } else {
      try {
        val id = this.insert(doc)
        Success(doc.copy(id = Some(id)))
      } catch {
        case e: Throwable => Failure(e)
      }
    }
  }

  /**
   * Retrieve all {@link Document}s of a given <em>category</em>.
   *
   * @param category The { @link DocumentType} the { @link Document}s must have in order to be retrieved.
   */
  def getAllByCategory(category: DocumentType): Validation[Throwable, List[Document]] = {
    try {
      val docs = for {
        doc <- Documents
        if doc.category === category
      } yield doc
      Success(docs.list)
    } catch {
      case e: Throwable => Failure(e)
    }
  }

  def getSome(int: limit): Validation[Throwable, List[Document]] = {
    try {
//      Documents.
    } catch {
      case e: Throwable => Failure(e)
    }
  }
}