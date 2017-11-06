package com.ngs.sort.merge

object MergeSortApp extends App {

  object SeqMerge {
    import scala.collection.mutable.PriorityQueue
    import scala.collection.mutable.ListBuffer
    import scala.math.Ordering

    class OrderingMA[A, M <: Seq[A]]( ord: Ordering[A] ) extends Ordering[M] {
      override def compare( as: M, bs: M ): Int = ( as, bs ) match {
        case ( xs, ys ) if ( xs.isEmpty || ys.isEmpty ) => throw new RuntimeException("Match error: empty Seq.")
        case _ => ord.compare( as.head, bs.head )
      }
    }

    def apply() = {
      val rnd = scala.util.Random
      implicit val ordma = new OrderingMA[ Int, Seq[Int] ]( Ordering.Int.reverse )

      val data = Seq(
        Seq.fill( 15 )( rnd.nextInt( 15 ) ).sorted,
        Seq.fill( 15 )( rnd.nextInt( 15 ) ).sorted,
        Seq.fill( 15 )( rnd.nextInt( 15 ) ).sorted
      )

      val res = merge[Int, Seq[Int]]( data )
      println( s"res ${res}" )
    }

    def merge[A, M <: Seq[A]]( data: Seq[M] )( implicit ord: OrderingMA[A, M] ): Seq[A] = {

      val que = data.foldLeft( PriorityQueue.empty[M] ) { (acc, ms) => ms match {
        case m: M if ( m.isEmpty ) => acc
        case m: M =>
          acc.enqueue( m )
          acc
      }}

      val buf = ListBuffer.empty[A]
      while ( !que.isEmpty ) que.dequeue match {
        case ::( x: A, ms: M ) =>
          buf += x
          if ( !ms.isEmpty ) que.enqueue( ms )
        case e => throw new RuntimeException(s"Match error. ${e.toString()}")
      }

      buf.toSeq
    }

  }

  SeqMerge()

}
