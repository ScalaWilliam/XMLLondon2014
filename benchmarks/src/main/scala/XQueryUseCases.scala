import java.io.FileNotFoundException

import scala.util.Try
import scala.xml.Node
import scala.xml.PrettyPrinter
import scala.xml.XML

import com.felstar.xqs.XQS._

import javax.xml.xquery.XQConstants
import javax.xml.xquery.XQResultSequence

// using http://www.w3.org/TR/xquery-use-cases/ section 1.1

object XQueryUseCases extends App {

  def loadXML(xmlName:String)={
    val is=getClass.getResourceAsStream(xmlName)
    try{
     if (is!=null) XML.load(is) else throw new FileNotFoundException(xmlName)
    } finally {
     if (is!=null) is.close()   
    }
  }
  
 val conn:javax.xml.xquery.XQConnection =  ???
 
 val usePrep=true
 val serializeResults=true
 
  val pp=new PrettyPrinter(80,2).format(_:Node)
  
  implicit class MyNodeSeq(ns:scala.xml.NodeSeq){
	    def ===(st:String)=ns.text==st
	    def sortedByText=ns.map(_.text).sorted
	    def sortByText(st:String)=ns.sortBy(x=>(x\st).text)
	    def groupByText(that:String)=ns.groupBy(x=>(x\that).text)
	    def groupByOrderBy(that:String)=groupByText(that).toList.sortBy(_._1)
	    def textSet=ns.map(_.text).toSet
	    def \@(name:String)= ns map (_\s"""@$name""")
	    //def filterByText(that:String)=ns.groupBy(x=>(x\that).text)
	  }	  
  implicit def nodeSeq2Int(ns:scala.xml.NodeSeq)=Try{ns.text.toInt}.getOrElse(Int.MinValue)	  
  implicit def elem2Seq(elem:scala.xml.Elem)=Seq(elem)

  

  val domSourceMap=scala.collection.mutable.Map[scala.xml.Elem,javax.xml.transform.dom.DOMSource]()
  
  case class Query(q:String){
    lazy val prep=conn.prepareExpression(q)
    def getDomSource(xml:scala.xml.Elem)=domSourceMap.getOrElseUpdate(xml, toDomSource(xml))
    def executeQuery(xml:scala.xml.Elem)=prep.document(XQConstants.CONTEXT_ITEM,getDomSource(xml:scala.xml.Elem)).executeQuery()
  }
  
  def runXQuery(query:Query)(xml:scala.xml.Elem)={
    if (usePrep) query.executeQuery(xml) else conn(query.q,xml) 
  }
  
  val Q1x= runXQuery(Query("""<bib>
	 {
	  for $b in /bib/book
	  where $b/publisher = "Addison-Wesley" and $b/@year > 1991
	  return
	    <book year="{ $b/@year }">
	     { $b/title }
	    </book>
	 }
	</bib>""")) _
  
  def Q1s(xml:scala.xml.Elem)={
      <bib> {for {b<-xml\"book"	  
	    if  b\"publisher"==="Addison-Wesley" && b\"@year">1991
	    } yield <book year={b\"@year"}>
             {b\"title"}
           </book>} </bib>
  }
  
  
 val Q2x= runXQuery(Query("""<results>
  {
    for $b in /bib/book,
        $t in $b/title,
        $a in $b/author
    return
        <result>
            { $t }    
            { $a }
        </result>
  }
</results>""")) _
  
  def Q2s(xml:scala.xml.Elem)={
      <results> {for {b<-xml\"book"
        t<-b\"title"
        a<-b\"author"	    
	    } yield <result>
             {t} 
	    	 {a}
           </result>} </results>
  }
  
   val Q3x=runXQuery(Query("""<results>
{
    for $b in /bib/book
    return
        <result>
            { $b/title }
            { $b/author  }
        </result>
}
</results>""")) _
  
  def Q3s(xml:scala.xml.Elem)={
      <results> {for {b<-xml\"book"   
	    } yield <result>
             {b\"title"} 
	    	 {b\"author"}
           </result>} </results>
  }
  
    val Q4x=runXQuery(Query("""<results>
  {
    let $a := //author
    for $last in distinct-values($a/last),
        $first in distinct-values($a[last=$last]/first)
    order by $last, $first
    return
        <result>
            <author>
               <last>{ $last }</last>
               <first>{ $first }</first>
            </author>
            {
                for $b in /bib/book
                where some $ba in $b/author 
                      satisfies ($ba/last = $last and $ba/first=$first)
                return $b/title
            }
        </result>
  }
</results>""")) _
  
  def Q4s(xml:scala.xml.Elem)={
      <results> {
        val a=xml\\"author"
        for {last<-(a\"last").sortedByText.distinct
            myas=a(auth=>(auth\"last")===last)
        	first<-(myas \"first").sortedByText.distinct
	    } yield <result>
           <author>
               <last>{ last }</last>
               <first>{ first }</first>               
            </author> 
             {
               for {b<-xml\"book"
                   auth<- b\"author"
                   if (auth\"last")===last && (auth\"first")===first
               } yield {b\"title"}
             }
           </result>} </results>
  }
  
    def Q4s2(xml:scala.xml.Elem)={
      <results> {
        // less work, map of lastname to author node, sorted by last name.
        for {(last,myas)<- (xml\\"author").groupByOrderBy("last")
        	first<-(myas \"first").sortedByText.distinct
	    } yield <result>
           <author>
               <last>{ last }</last>
               <first>{ first }</first>               
            </author> 
             {
               for {b<-xml\"book"
                   auth<- (b\"author") 
                   if (auth\"last")===last && (auth\"first")===first
               } yield {b\"title"}
             }
           </result>} </results>
  }
  
   val Q5x=runXQuery(Query("""<books-with-prices>
  {
    for $b in /root/bib/book,
        $a in /root/reviews/entry
    where $b/title = $a/title
    return
        <book-with-prices>
            { $b/title }
            <price-bstore2>{ $a/price/text() }</price-bstore2>
            <price-bstore1>{ $b/price/text() }</price-bstore1>
        </book-with-prices>
  }
</books-with-prices>""")) _
  
  def Q5s(xml:scala.xml.Elem)={
      <books-with-prices> 
	  {for {b<-xml\"bib"\"book"
	    a<-xml\"reviews"\"entry"
	    if (a\"title")==(b\"title")
	    } yield <book-with-prices>
             { b\"title" }
            <price-bstore2>{ a\"price" text }</price-bstore2>
            <price-bstore1>{ b\"price" text }</price-bstore1>
           </book-with-prices>} </books-with-prices>
  }  
    
  def Q5s2(xml:scala.xml.Elem)={
      <books-with-prices> 
	  { // making a map and a tuple2 seq (filtered on title existing in map)
	    // a bit more code but quite a bit faster
	    val prices=(xml\"reviews"\"entry").map(e=>((e\"title").text->(e\"price").text)).toMap
	    val bseq=(xml\"bib"\"book").map(b=>((b\"title").text->(b\"price").text)).filter(pair=>prices.contains(pair._1))	    
	    for ((title,price)<-bseq)	      	    
	     yield <book-with-prices>
             <title>{ title }</title>
            <price-bstore2>{ prices(title)}</price-bstore2>
            <price-bstore1>{ price }</price-bstore1>
           </book-with-prices>
      } </books-with-prices>
  }
  
  val Q6x=runXQuery(Query("""<bib>
  {
    for $b in /bib/book
    where count($b/author) > 0
    return
        <book>
            { $b/title }
            {
                for $a in $b/author[position()<=2]  
                return $a
            }
            {
                if (count($b/author) > 2)
                 then <et-al/>
                 else ()
            }
        </book>
  }
</bib>""")) _
  
  def Q6s(xml:scala.xml.Elem)={
      <bib> 
	  {for {b<-xml\"book"
	    authors=b\"author"
	    if authors.size>0	    
	  } yield 
	    <book>
            { b\"title" }
            {
              authors.take(2)
            }
            {
              if (authors.size>2) <et-al/> else ()
            }
        </book>} 
	  </bib>
  }  
  
  val Q7x=runXQuery(Query("""<bib>
  {
    for $b in /bib/book
    where $b/publisher = "Addison-Wesley" and $b/@year > 1991
    order by $b/title
    return
        <book>
            { $b/@year }
            { $b/title }
        </book>
  }
</bib> """)) _
  
  def Q7s(xml:scala.xml.Elem)={
      <bib> {for {b<-(xml\"book").sortByText("title")	  
	    if  b\"publisher"==="Addison-Wesley" && b\"@year">1991
	    } yield <book year={b\"@year"}>
             {b\"title"}
           </book>} </bib>
  }  
    
  val Q9x=runXQuery(Query("""
    <results>
  {
    for $t in //(chapter | section)/title
    where contains($t/text(), "XML")
    return $t
  }
</results> 
    """)) _
  
  def Q9s(xml:scala.xml.Elem)={
     <results> 
	  {
	   for {t<-(xml\\"_")(n=> Seq("chapter","section").contains(n.label)) \"title"
	      if (t.text.contains("XML")) 
	        } 
	     yield t
	  }
 	</results>
  }
  
   val Q10x=runXQuery(Query("""
    <results>
  {    
    for $t in distinct-values(/prices/book/title)
    let $p := /prices/book[title = $t]/price
    return
      <minprice title="{ $t }">
        <price>{ min($p) }</price>
      </minprice>
  }
</results> 
   """)) _
  
  def Q10s(xml:scala.xml.Elem)={
     <results> 
	  {
	   for {t<- (xml\"book"\"title").map(_.text).distinct
	       price=((xml\"book").filter(b=>(b\"title").text==t)\"price").map(_.text.toDouble).min
	        } 
	     yield		     
	     <minprice title={t}>
	     	<price>{ price}</price>
	     	</minprice>	           
	  }
 	</results>
  }
  
   val Q11x=runXQuery(Query("""
    <bib>
{
        for $b in /bib/book[author]
        return
            <book>
                { $b/title }
                { $b/author }
            </book>
}
{
        for $b in /bib/book[editor]
        return
          <reference>
            { $b/title }
            {$b/editor/affiliation}
          </reference>
}
</bib>  
   """)) _
  
  def Q11s(xml:scala.xml.Elem)={
    val books=xml\"book"
     <bib> 
	  {
	   for (b<-books;
	        author=b\"author"
	       if !author.isEmpty
	       )
	     yield <book>
	     	{b\"title"}
		    {author}
	     	</book>
	  }
	  {
	   for (b<-books;
		   editor=b\"editor"
	       if !editor.isEmpty
	       )
	     yield <reference>
	     	{b\"title"}
		    {editor\"affiliation"}
	     	</reference>
	  }
 	</bib>
  }
  
  val Q4_3x=runXQuery(Query("""
    <result>{
  for $store in /root/*/store
  let $state := $store/state
  group by $state
  order by $state
  return
    <state name="{$state}">{
      for $product in /root/*/product
      let $category := $product/category
      group by $category
      order by $category
      return
        <category name="{$category}">{
          for $sales in /root/*/record[store-number = $store/store-number
            and product-name = $product/name]
          let $pname := $sales/product-name
          group by $pname
          order by $pname
          return
            <product name="{$pname}" total-qty="{sum($sales/qty)}" />
          }</category>
    }</state>
}</result>
   """)) _
  
   def Q4_3s(xml:scala.xml.Elem)={
      val allStores=(xml\"_"\"store").groupByOrderBy("state")
      val allProducts=(xml\"_"\"product").groupByOrderBy("category")
	  val allRecords=(xml\"_"\"record").groupByOrderBy("product-name").toMap
     
     <result> 
	  {
	    for {(state,stores)<- allStores
	    storeNumbers=(stores\"store-number").textSet    	  
	    }
	     yield <state name={state}>
	     	{
	     	  for {(category,products)<- allProducts	     	  
	     	  productRecords=allRecords.filterKeys{(products\"name").textSet}
	     	  }
			  yield <category name={category}>{
				 for {(pname,sales)<- productRecords
					 filteredSales=sales.filter(n=>storeNumbers(n\"store-number" text) )	     	   
					 
					 if !filteredSales.isEmpty
				 }
			 yield <product name={pname} total-qty={(filteredSales\"qty").map(_.text toInt).sum.toString}/>}</category>
	     	}
	     	</state>
	  }
	  </result>
  }
  
  type XTransform = scala.xml.Elem=>XQResultSequence
  type STransform = scala.xml.Elem=>scala.xml.Elem
  
  def test(xml:scala.xml.Elem,expected:scala.xml.Elem,qx: XTransform, qs: STransform)={	  
      {        
       val out:Seq[scala.xml.Elem]=qx(xml) 
       val diff=new org.custommonkey.xmlunit.Diff(expected: org.w3c.dom.Document,out.head: org.w3c.dom.Document)      
	   assert(diff.similar(),s"XQuery: $diff $out ---- $expected")
      }
      {
       val out=qs(xml)
       val diff=new org.custommonkey.xmlunit.Diff(expected: org.w3c.dom.Document,out: org.w3c.dom.Document)
       assert(diff.similar(),s"Scala: $diff $out ---- $expected")
      }
  }
 
  def time(loops:Int)(f: => Any):Long={
    val time=System.currentTimeMillis()
    for (x<-0 to loops)        
     f    
    val span=(System.currentTimeMillis()-time)
    //println(span)
    span
  }
  
  case class Timings(xelapsed:Long,selapsed:Long){
    def +(other:Timings)=
      Timings(xelapsed+other.xelapsed,selapsed+other.selapsed)    
    
    lazy val toStringExtra=toString()+s" ratio="+ratioFormatted
    
    lazy val ratioFormatted="%1.2f" format ratio
    
    val ratio:Double=if (selapsed!=0) xelapsed.toDouble/selapsed else 0
  }
  
  def runTest(xml:scala.xml.Elem,expected:scala.xml.Elem,qx: XTransform, qs: STransform)=
  {
    // check that they both emit expected xml, once, then do loops for timings
   test(xml,expected,qx,qs)
	  
   val loops=10000
  
   val timings=for {_<-1 to 3;
     
	   xelapsed=time(loops){
		    val res=qx(xml)
		  //  if (parseXQueryResults) toSeqXML(res).head
		    val str=if (serializeResults) toSeqString(res).head else ""

	   }
	   
	   selapsed=time(loops){
		    val out=qs(xml)
		    if (out.isEmpty) println ("Empty!!!")
		    val str=if (serializeResults) out.toString else ""
	   }
   } yield Timings(xelapsed,selapsed)
    // throw away first 2 runs
   timings.last
  }
 
  lazy val bibXML=loadXML("xml/bib.xml")
  lazy val reviewsXML=loadXML("xml/reviews.xml")
  lazy val booksXML=loadXML("xml/books.xml")
  lazy val pricesXML=loadXML("xml/prices.xml")
  lazy val storesXML=loadXML("xml/stores.xml")
  lazy val productsXML=loadXML("xml/products.xml")
  lazy val salesXML=loadXML("xml/sales-records.xml")
  
  org.custommonkey.xmlunit.XMLUnit.setIgnoreWhitespace(true)
  
  var totals=Timings(0,0)
  
  def sum(timings:Timings)={
     println(s"\t${timings.ratioFormatted}\t${timings.xelapsed}\t${timings.selapsed}")
     totals=totals+timings
  }
  println(s"Query\tRatio\tXQuery\tScala")
  
  print("Q1")
  sum(runTest(bibXML,loadXML("xml/Q1_expected.xml"),Q1x,Q1s))
  print("Q2")
  sum(runTest(bibXML,loadXML("xml/Q2_expected.xml"),Q2x,Q2s))
  print("Q3")
  sum(runTest(bibXML,loadXML("xml/Q3_expected.xml"),Q3x,Q3s))
  print("Q4")
  sum(runTest(bibXML,loadXML("xml/Q4_expected.xml"),Q4x,Q4s2))
  print("Q5")
  sum(runTest(<root>{bibXML}{reviewsXML}</root>,loadXML("xml/Q5_expected.xml"),Q5x,Q5s2))
  print("Q6")
  sum(runTest(bibXML,loadXML("xml/Q6_expected.xml"),Q6x,Q6s))
  print("Q7")
  sum(runTest(bibXML,loadXML("xml/Q7_expected.xml"),Q7x,Q7s))
  print("Q9")
  sum(runTest(booksXML,loadXML("xml/Q9_expected.xml"),Q9x,Q9s))
  print("Q10")
  sum(runTest(pricesXML,loadXML("xml/Q10_expected.xml"),Q10x,Q10s))
  print("Q11")
  sum(runTest(bibXML,loadXML("xml/Q11_expected.xml"),Q11x,Q11s))
  
  print("Q4_3.0")
  sum(runTest(<root>{storesXML}{productsXML}{salesXML}</root>,loadXML("xml/Q4_3.0_expected.xml"),Q4_3x,Q4_3s))
  
  println (totals.toStringExtra)
  
  conn.close()
}