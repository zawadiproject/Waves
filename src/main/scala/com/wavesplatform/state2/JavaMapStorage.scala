package com.wavesplatform.state2

import java.util

import org.h2.mvstore.{MVMap, MVStore}


trait JavaMapStorage {
  val transactions: java.util.Map[Array[Byte], (Int, Array[Byte])]
  val portfolios: java.util.Map[Array[Byte], (Long, Long, Map[Array[Byte], Long])]
  val assets: java.util.Map[Array[Byte], (Boolean, Long)]
  val accountTransactionIds: java.util.Map[Array[Byte], Array[Byte]]

  def getHeight: Int

  def setHeight(i: Int): Unit
}

class MVStorePrimitiveImpl(db: MVStore) extends JavaMapStorage {
  val transactions: MVMap[Array[Byte], (Int, Array[Byte])] = db.openMap("txs")

  val portfolios: MVMap[Array[Byte], (Long, Long, Map[Array[Byte], Long])] = db.openMap("portfolios")

  val assets: MVMap[Array[Byte], (Boolean, Long)] = db.openMap("assets")

  val accountTransactionIds : MVMap[Array[Byte], Array[Byte]] = db.openMap("accountTransactionIds")

  private val variables: MVMap[String, Int] = db.openMap("variables")
  private val heightKey = "height"

  def getHeight: Int = variables.get(heightKey)

  def setHeight(i: Int): Unit = variables.put(heightKey, i)


}
