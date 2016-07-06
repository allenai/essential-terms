package org.allenai.ari.solvers.termselector

import com.redis.RedisClient
import org.allenai.common.Logging

/** a thread-safe wrapper for redis-client */
class SynchronizedRedisClient(host: String = "localhost") extends Logging {
  private lazy val redisClient = try {
    new RedisClient(host, 6379)
  } catch {
    case e: RuntimeException =>
      throw new Exception("Redis is not running, although it is set to be used in" +
        " \"application.conf\" . . . ")
  }

  def redisGet(key: String): Option[String] = {
    try {
      this.synchronized(redisClient.get(key))
    } catch {
      case e: Exception => {
        e.printStackTrace()
        throw new Exception(s"Fetching information from redis failed for key: $key!")
      }
    }
  }

  def redisSet(key: String, value: String): Unit = {
    try {
      this.synchronized(redisClient.set(key, value))
    } catch {
      case e: Exception => {
        e.printStackTrace()
        throw new Exception(s"Setting information in redis failed for key: $key")
      }
    }
  }

  def keys(): Option[List[Option[String]]] = {
    try {
      this.synchronized(redisClient.keys())
    } catch {
      case e: Exception =>
        e.printStackTrace()
        throw new Exception("Fetching all keys from redis failed!")
    }
  }
}

/** a dummy redis client for when redis is not supposed to be used */
object DummyRedisClient extends SynchronizedRedisClient("") {
  override def redisGet(key: String) = None
  override def redisSet(key: String, value: String) = None
  override def keys() = None
}
