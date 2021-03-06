package objsets

import org.junit._
import org.junit.Assert.assertEquals


class TweetSetSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val setMost1 = set1.incl(c)
    val setMost2 = setMost1.incl(d)
    val e = new Tweet("e", "e body", 8)
    val f = new Tweet("f", "f body", 11)
    val g = new Tweet("g", "g body", 22)
    val h = new Tweet("h", "h body", 1)
    val setDesc1 = set1.incl(h).incl(c).incl(f).incl(d)
    val setDesc2 = set1.incl(f).incl(d).incl(g).incl(e)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  @Test def `filter: on empty set`: Unit =
    new TestSets {
      assertEquals(0, size(set1.filter(tw => tw.user == "a")))
    }

  @Test def `filter: a on set5`: Unit =
    new TestSets {
      assertEquals(1, size(set5.filter(tw => tw.user == "a")))
    }

  @Test def `filter: not a on set5`: Unit =
    new TestSets {
      assertEquals(3, size(set5.filter(tw => tw.user != "a")))
    }

  @Test def `filter: twenty on set5`: Unit =
    new TestSets {
      assertEquals(2, size(set5.filter(tw => tw.retweets == 20)))
    }

  @Test def `filter: < 100 on set5`: Unit =
    new TestSets {
      assertEquals(4, size(set5.filter(tw => tw.retweets < 100)))
    }

  @Test def `union: set4c and set4d`: Unit =
    new TestSets {
      assertEquals(4, size(set4c.union(set4d)))
    }

  @Test def `union: with empty set1`: Unit =
    new TestSets {
      assertEquals(4, size(set5.union(set1)))
    }

  @Test def `union: with empty set2`: Unit =
    new TestSets {
      assertEquals(4, size(set1.union(set5)))
    }

  @Test def `union: set2 and set3`: Unit =
    new TestSets {
      assertEquals(2, size(set2.union(set3)))
    }

  @Test def `union: set3 and set2`: Unit =
    new TestSets {
      assertEquals(2, size(set3.union(set2)))
    }

  @Test def `mostRetweeted: set3`: Unit =
    new TestSets {
      assertEquals(20, set3.mostRetweeted.retweets)
    }

  @Test def `mostRetweeted: set4c`: Unit =
    new TestSets {
      assertEquals(20, set4c.mostRetweeted.retweets)
    }

  @Test def `mostRetweeted: set4d`: Unit =
    new TestSets {
      assertEquals(20, set4d.mostRetweeted.retweets)
    }

  @Test def `mostRetweeted: set5`: Unit =
    new TestSets {
      assertEquals(20, set5.mostRetweeted.retweets)
    }

  @Test def `mostRetweeted: setMost1`: Unit =
    new TestSets {
      assertEquals(7, setMost1.mostRetweeted.retweets)
    }

  @Test def `mostRetweeted: set2`: Unit =
    new TestSets {
      assertEquals(9, setMost2.mostRetweeted.retweets)
    }

  @Test(expected = classOf[java.util.NoSuchElementException])
  def `mostRetweeted: empty throws java.util.NoSuchElementException`: Unit =
    new TestSets {
      set1.mostRetweeted
    }

  @Test def `descending: set5`: Unit =
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }

  @Test def `descending: setDesc1`: Unit =
    new TestSets {
      val trends = setDesc1.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "f")
    }

  @Test def `descending: setDesc2`: Unit =
    new TestSets {
      val trends = setDesc2.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "g")
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
