package org.allenai.ari.solvers.termselector

import com.redis.RedisClient
import org.allenai.common.Logging

/** a thread-safe wrapper for redis-client */
class SynchronizedRedisClient(host: String = "localhost") extends Logging {
  // TODO(daniel): create a key in application.conf for activating this feature
  private val redisClientOpt = try {
    Some(new RedisClient(host, 6379))
  } catch {
    case e: RuntimeException =>
      logger.warn("Redis is not running . . . ")
      None
  }

  def redisGet(key: String): Option[String] = {
    redisClientOpt.flatMap {
      redisClient =>
        try {
          this.synchronized(redisClient.get(key))
        } catch {
          case e: Exception => {
            e.printStackTrace()
            throw new Exception(s"Fetching information from redis failed for ket: $key!")
          }
        }
    }
  }

  def redisSet(key: String, value: String): Unit = {
    redisClientOpt.map {
      redisClient =>
        try {
          this.synchronized(redisClient.set(key, value))
        } catch {
          case e: Exception => {
            e.printStackTrace()
            throw new Exception(s"Setting information in redis failed for key: $key")
          }
        }
    }
  }

  def keys(): Option[List[Option[String]]] = {
    redisClientOpt.flatMap {
      redisClient =>
        try {
          this.synchronized(redisClient.keys())
        } catch {
          case e: Exception =>
            e.printStackTrace()
            throw new Exception("Fetching all keys from redis failed!")
        }
    }
  }
}
