package uk.ac.cdrc.data.spark.jdbc

/**
  * Created  on 11/24/16.
  */
import java.io.InputStream
import java.sql.Connection
import java.util.Properties

import org.apache.spark.sql.execution.datasources.jdbc.JdbcUtils
import org.apache.spark.sql.{DataFrame, Row}
import org.postgresql.copy.CopyManager
import org.postgresql.core.BaseConnection

class JDBCWriter(val jdbcUrl: String, connProps: Map[String, String]){
  //jdbcUrl = s"jdbc:postgresql://..." // db credentials elided
  val connectionProperties: Properties = {
    val props = new java.util.Properties()

    for { (k, v) <- connProps}
      props.setProperty("driver", "org.postgresql.Driver")

    props
  }

  // Spark reads the "driver" property to allow users to override the default driver selected, otherwise
  // it picks the Redshift driver, which doesn't support JDBC CopyManager.
  // https://github.com/apache/spark/blob/v1.6.1/sql/core/src/main/scala/org/apache/spark/sql/execution/datasources/jdbc/JdbcUtils.scala#L44-51
  val cf: () => Connection = JdbcUtils.createConnectionFactory(jdbcUrl, connectionProperties)

  // Convert every partition (an `Iterator[Row]`) to bytes (InputStream)
  def rowsToInputStream(rows: Iterator[Row], delimiter: String): InputStream = {

    val bytes: Iterator[Byte] = for {
      row <- rows
      b <- (row.mkString(delimiter) + "\n").getBytes
    } yield b

    new InputStream {
      override def read(): Int = if (bytes.hasNext) {
        bytes.next & 0xff // bitwise AND - make the signed byte an unsigned int from 0-255
      } else {
        -1
      }
    }
  }

  def write(frame: DataFrame, table: String):Unit = {
    // Beware: this will open a db connection for every partition of your DataFrame.
    frame.foreachPartition { rows =>
      val conn = cf()
      val cm = new CopyManager(conn.asInstanceOf[BaseConnection])

      cm.copyIn(
        s"""COPY $table FROM STDIN WITH (NULL 'null', FORMAT CSV, DELIMITER E'\t')""", // adjust COPY settings as you desire, options from https://www.postgresql.org/docs/9.5/static/sql-copy.html
        rowsToInputStream(rows, "\t"))

      conn.close()
    }
  }
}
