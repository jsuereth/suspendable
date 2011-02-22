package scalax.collection.suspendable

import collection.generic.CanBuildFrom
import collection.IterableLike
import util.continuations._

/** 
 * This class represents a DelimitedContinaution friendly proxy of an IterableCollection.
 *
 * @tparam T  The type of elements in the proxied collection
 * @tparam Repr The most specific known type of the collection.
 * @param xs  The proxied collection.
 * @param cbf A captured builder that can build the original collection type.
 *
 * @author jsuereth
 *
 * @define coll suspendable Iterable like collection
 */
class SuspendableIterableLikeProxy[+T, +Repr](val xs: IterableLike[T, Repr])(implicit cbf : CanBuildFrom[Repr, T, Repr]) {
  private[this] def newBuilder(from : Repr) = cbf(from)
  private[this] def newBuilder() = cbf()
  /** Applies a function `f` to all elements of this $coll.
   *
   *    Note: this method underlies the implementation of most other bulk operations.
   *    Subclasses should re-implement this method if a more efficient implementation exists.
   *
   *  @usecase def foreach[A](f: T => Unit): Unit @cps[A]
   */
  def foreach[U, A](body: T => U @cps[A]): Unit @cps[A] = {
    val it = xs.iterator
    while (it.hasNext) {
      body(it.next)
    }
  }

  /**
   * @return true if the $coll is empty.
   */
  def isEmpty = xs.isEmpty

  /** Builds a new collection by applying a function to all elements of this $coll.
   *
   *  @param f      the function to apply to each element.
   *  @tparam A     the return type of the final delimited continuation.
   *  @tparam B     the element type of the returned collection.
   *  @tparam To    the collection type returned.
   *  @param bf     A builder that we use to construct the new collection.
   *  @return       a new collection of type `That` resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results.
   *
   *  @usecase def map[A,B](f: T => B @cps[A]): Iterable[B] @cps[A]
   *
   *  @return       a new $coll resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results.  This operation could be
   *                suspended at any point and is not guaranteed to complete its pass through the collection.
   */
  def map[B, A, To](f : T => B @cps[A])(implicit bf : CanBuildFrom[Repr, B, To]) : To @cps[A] = {
    val b = bf(xs.repr)
    val it = xs.iterator
    while (it.hasNext) {
      b += f(it.next)
    }
    b.result
  }
  /** Selects all elements of this $coll which satisfy a predicate.
   *
   *  @tparam A    the eventual return value of the delimited continuation.
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
   *               predicate `p`. The order of the elements is preserved.  This operation could be
   *                suspended at any time.
   */
  /*def filter[A](f : T => Boolean @cps[A]) : Repr @cps[A] = {
    val it = xs.iterator
    val b = newBuilder(xs.repr)
    while (it.hasNext) {
      val elem = it.next
      if (f(elem))
        b += elem
    }
    b.result
  }*/

  /** Selects all elements of this $coll which do not satisfy a predicate.
   *
   *  @tparam A    the eventual return value of the delimited continuation.
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  /*def filterNot[A](p: T => Boolean @cps[A]): Repr @cps[A] = filter(!p(_))*/

  /** Builds a new collection by applying a function to all elements of this $coll
   *  and concatenating the results.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam To    The resulting collection type
   *  @param bf     An implicit that determines the resulting collection type.
   *  @return       a new collection of type `That` resulting from applying the given collection-valued function
   *                `f` to each element of this $coll and concatenating the results.
   *
   *  @usecase def flatMap[B, A](f: A => TraversableOnce[B]): Iterable[B] @cps[A]]
   *
   *  @return       a new $coll resulting from applying the given collection-valued function
   *                `f` to each element of this $coll and concatenating the results.  This operation could be
   *                suspended at any time.
   */
  def flatMap[B, A, To](f : T => TraversableOnce[B] @cps[A])(implicit bf : CanBuildFrom[Repr, B, To]) : To @cps[A] = {
    val it = xs.iterator
    val b = bf(xs.repr)
    while (it.hasNext) {
      b ++= f(it.next)
    }
    b.result
  }

  // TODO(jsuereth): Figure out how to make collect work with CPS.

  /** Partitions this $coll in two ${coll}s according to a predicate.
   *
   *  @param p the predicate on which to partition.
   *  @return  a pair of ${coll}s: the first $coll consists of all elements that
   *           satisfy the predicate `p` and the second $coll consists of all elements
   *           that don't. The relative order of the elements in the resulting ${coll}s
   *           is the same as in the original $coll.
   */
  /*def partition[A](p: T => Boolean @cps[A]): (Repr, Repr) @cps[A] = {
    val l, r = newBuilder()
    val it = xs.iterator
    while (it.hasNext) {
      val x = it.next
      (if (p(x)) l else r) += x
    }
    (l.result, r.result)
  }*/

  /** Tests whether a predicate holds for all elements of this $coll.
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for all elements
   *                 of this $coll, otherwise `false`.
   */
  def forall[A](p: T => Boolean @cps[A]): Boolean @cps[A] = {
    var result = true
    val it = xs.iterator
    while (it.hasNext && result) {
      if (!p(it.next)) {
        result = false
      }
    }
    result
  }
  /** Tests whether a predicate holds for some of the elements of this $coll.
   *
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for some of the
   *                 elements of this $coll, otherwise `false`.
   */
  def exists[A](p: T => Boolean @cps[A]): Boolean @cps[A] = {
    var result = false
    val it = xs.iterator
    while (it.hasNext && !result) {
      if (p(it.next)) {
        result = true
      }
    }
    result
  }
  // TODO(jsuereth): implement find when we can.
}

object SuspendableIterableLikeProxy {
  def apply[T, Repr](xs : IterableLike[T, Repr])(implicit cbf : CanBuildFrom[Repr, T, Repr]) =
    new SuspendableIterableLikeProxy(xs)(cbf)

  /**This implicit lets users of the suspendable proxies revert to the original collection
   * as needed.
   *
   * TODO(jsuereth): This is probably a bad idea in the long run.
   */
  implicit def unwrap[T, Repr](xs : SuspendableIterableLikeProxy[T, Repr]) : Repr = xs.xs.repr
}