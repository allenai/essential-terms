package org.allenai.ari.solvers.termselector

import com.redis.RedisClient
import org.allenai.common.Logging

/** a thread-safe wrapper for redis-client */
class SynchronizedRedisClient(host: String = "localhost") extends Logging {
  // TODO(daniel): create a key in application.conf for activating this feature
  private val annotationRedisCache = new RedisClient(host, 6379)

  def redisGet(key: String): Option[String] = {
    try {
      this.synchronized(annotationRedisCache.get(key))
    } catch {
      case e: Exception => {
        logger.error("Fetching information from redis failed!")
        logger.error(s"Key: $key")
        e.printStackTrace()
        None
      }
    }
  }

  def redisSet(key: String, value: String): Unit = {
    try {
      this.synchronized(annotationRedisCache.set(key, value))
    } catch {
      case e: Exception => {
        logger.error("Setting information in redis failed!")
        logger.error(s"Key: $key")
        e.printStackTrace()
        None
      }
    }
  }

  def keys(): Option[List[Option[String]]] = {
    try {
      this.synchronized(annotationRedisCache.keys())
    } catch {
      case e: Exception =>
        logger.error("Fetching all keys from redis failed!")
        e.printStackTrace()
        None
    }
  }

}
