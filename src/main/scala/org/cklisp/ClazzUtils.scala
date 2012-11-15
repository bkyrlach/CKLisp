package org.cklisp

import java.lang.reflect.{Array => _, _}
import java.lang.Class
import scala.collection.mutable.HashMap
import scala.collection.immutable.Queue

sealed trait Relation[+T]
case class EXACT[T](obj: T) extends Relation[T] 
case class COMPATIBLE[T](obj: T) extends Relation[T]
case object NOREL extends Relation[Nothing]

object ClazzUtils {
  val topClass = (new Object).getClass()
  val methodCache = new HashMap[String, Relation[_]]();
  
  def createCacheKey(clazz: Class[_], methodName: String, args: List[Any]): String = {
    val params = args.foldLeft("")((a, b)=> a + (if(b == null) "null" else b.getClass.getName))
    clazz.getName+methodName+params
  }
  
  def addCache[T](key: String, rel: Relation[T]): Relation[T] = {
    methodCache+=(key -> rel)
    rel
  }
  
  def findBestMatchMethod(clazz: Class[_], methodName: String, args: List[Any]): Relation[Method] = {
	  val cacheKey=createCacheKey(clazz, methodName, args)
	  val cached = methodCache.get(cacheKey)
	  cached match {
	    case Some(v) => v.asInstanceOf[Relation[Method]] 
	    case None => {
	      val methods=clazz.getDeclaredMethods().filter(_.getName.equals(methodName))
	      val compat=methods.foldLeft(Queue.empty[Relation[Method]])((a, b) => a+getMethodRelation(b, args)).filter(_ != NOREL)
	      val exacts = compat.filter(_ match { case EXACT(m) => true; case _ => false })
	      exacts.headOption match {
	        case Some(e) => addCache(cacheKey,e)
	        case None => {
	          val parentRelation = if(clazz.equals(topClass)) NOREL else findBestMatchMethod(clazz.getSuperclass(), methodName, args) 
	          parentRelation match {
	            case EXACT(p) => parentRelation
	            case _ => if(compat.isEmpty) parentRelation else compat.head
	          }
          }
	      }
      }
	  }
  }
  
  def findBestMatchConstructor[T](clazz: Class[T], args: List[Any]) : Relation[Constructor[T]] ={
    val cacheKey=createCacheKey(clazz, "__C", args)
    val cached = methodCache.get(cacheKey)
    cached match {
	    case Some(v) => v.asInstanceOf[Relation[Constructor[T]]] 
	    case None => {
	      val cons=clazz.getDeclaredConstructors()
	      val compat=cons.foldLeft(Queue.empty[Relation[Constructor[T]]])((a, b) => a+getConstructorRelation(b.asInstanceOf[Constructor[T]], args)).filter(_ != NOREL)
	      val exacts = compat.filter(_ match { case EXACT(m) => true; case _ => false })
	      exacts.headOption match {
	        case Some(e) => addCache(cacheKey,e)
	        case None => compat.head
	      }
      }
	  }
  }
  
  
  def testClass[T](c1: Class[_], c2: Class[_], obj: T): Relation[T] = {
    if(c1.equals(c2)) EXACT(obj)
    else if (c2.isAssignableFrom(c1)) COMPATIBLE(obj)
    else NOREL
  }
  
  def getConstructorRelation[T](cons: Constructor[T], args: List[Any]): Relation[Constructor[T]] = getRelation(cons, args)
  
  def getMethodRelation(method: Method, args: List[Any]): Relation[Method] = getRelation(method, args)

  def getRelation[T](t: T, args: List[Any]): Relation[T] = {
    val ptypes = t match {
      case m: Method => m.getParameterTypes()
      case c: Constructor[_] => c.getParameterTypes()
      case _ => Array()
    }
    if(ptypes.length == args.size){
      val zipArgs = args.zip(ptypes)
      val rels = zipArgs.map((a) => testClass(a._1.getClass,a._2, t))
      rels.filter(_ == NOREL).headOption.getOrElse{
        rels.filter(_ == COMPATIBLE(t)).headOption.getOrElse(EXACT(t))
      }
    }else{
      NOREL
    }
  }
}