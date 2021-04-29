//package choreo.frontend.widgets
//
//import choreo.frontend.widgets.{DomElem, DomNode}
//import Box.Block
//import org.scalajs.dom
//import org.scalajs.dom.{EventTarget, MouseEvent, html}
//
//import scala.scalajs.js.{JavaScriptException, UndefOr}
//
//
////panel boxes are the abstract entities which contain each panel displayed on the website
//abstract class Box[A](val title: String, dependency: List[Box[_]]){
//  type Block = DomElem //Selection[dom.EventTarget]
//
//  var wrap:DomElem = _
//
//  /**
//   * Creates a collapsable pannel
//   * */
//  protected def panelBox(parent:Block,
//                         visible:Boolean,
//                         headerStyle: List[(String,String)] = Nil,
//                         buttons:List[(Either[String,String], (()=>Unit,String) )] = Nil) : Block = {
//    //    val percentage=100
//
//    //var expander: Block = parent
//    wrap = parent.append("div").attr("class","panel-group")
//      .append("div").attr("class","panel panel-default").attr("id",title)
//    var expander = wrap
//      .append("div").attr("class", "panel-heading my-panel-heading")
//      .append("h4")
//      .attr("class", "panel-title")
//      .attr("style",s"padding-right: ${28*buttons.size}pt;")
//    //        .append("table")
//    //        .attr("width", "100%")
//    //        .append("th")
//    //      .attr("width", s"$percentage%")
//    for ((s,v)<-headerStyle)
//      expander.style(s,v)
//    expander = expander
//      .append("a").attr("data-toggle", "collapse")
//      .attr("href", "#collapse-1" + title.hashCode)
//      .attr("aria-expanded", visible.toString)
//    if(!visible)
//      expander.attr("class","collapsed")
//    expander
//      .text(title)
//    val res = wrap
//      .append("div").attr("id","collapse-1"+title.hashCode)
//      .attr("class",if (visible) "panel-collapse collapse in" else "panel-collapse collapse")
//      .attr("style",if (visible) "" else "height: 0px;")
//      .attr("aria-expanded",visible.toString)
//      .append("div").attr("class","panel-body my-panel-body")
//
//    // Buttons
//    for ((name,(action,title)) <- buttons.reverse) {
//      //      .append("button").attr("class", "btn btn-default btn-sm")
//      //        .style("float","right")
//      //        .style("margin-top","-15pt")
//      //        .style("display","flex")
//
//      val button = wrap
//        .select("div")
//        .append("button").attr("class", "btn btn-default btn-sm")
//        .style("float","right")
//        .style("margin-top","-15.5pt")
//        .style("max-height","18pt")
//        .style("margin-left","2pt")
//        .style("display","flex")
//      if (name==Right("help")) button
//        .style("margin-left","-2pt")
//        .style("border", "none")
//        .style("background", "none")
//        .style("box-shadow", "none")
//        .style("padding", "3pt")
//      if (title.nonEmpty) button.attr("title",title)
//
//      drawButton(button,name)
//
//      //      button.on("click", {(e: EventTarget, a: Int, b:UndefOr[Int])=> { action() }})
//      // EXPERIMENT
//      button.on("click", ()=>action() )
//    }
//
//    res
//  }
//
//  private def drawButton(button:Block, info:Either[String,String]): Unit = {
//    info match {
//      case Left(str) =>
//        val b = button.append("span")
//        b.style("line-height","9pt")
//        b.html(str)
//      case Right("download") =>
//        Box.downloadSvg(button)
//      //        val svg = button.append("img")
//      //          .attr("src","assets/content/svg/cloud_download.svg")
//      //          .style("width","15pt")
//      case Right("refresh") =>
//        button.append("span").attr("class", "glyphicon glyphicon-refresh")
//      case Right("help") =>
//        val b = button.append("span")
//        b .style("line-height","9pt")
//          .style("padding","0pt 0pt 4pt 3pt")
//          .style("color", "#b0b0b0")
//          .style("text-shadow", "none")
//        b.html("?")
//      //        button.append("img")
//      //          .attr("src","svg/help.svg")
//      //          .style("margin","-2pt -2pt 0pt -2pt")
//      case Right("oldDownload") => drawButton(button,Left("&dArr;"))
//      case Right(s) => drawButton(button,Left(s))
//    }
//  }
//
//  def isVisible: Boolean = {
//    val es = dom.document.getElementsByClassName("collapsed")
//    var foundId = false
//    for (i <- 0 until es.length) {
//      // println(es.item(i).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value)
//      //      println("### - "+es.item(i).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value)
//      foundId = foundId || es.item(i).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value == title
//    }
//
//    //    println("### - "+es.length)
//    //    println("### - "+es.item(0).localName)
//    //    println("### - "+es.item(0).parentNode.localName)
//    //    println("### - "+es.item(0).parentNode.parentNode.localName)
//    //    println("### - "+es.item(0).parentNode.parentNode.parentNode.attributes.getNamedItem("id").value)
//
//    //    val res = expander.attr("aria-expander") == "true"
//    //    println("--- "+expander.html().render)
//    //    println("--- "+expander.classed("collapsed"))
//    //    println("--- "+expander.attr("aria-expander"))
//    //    println("$$$ "+ (!foundId))
//    !foundId
//  }
//
//  // add actions in "init" to update to visibility toggles
//  def toggleVisibility(visible:()=>Unit = ()=>{}, invisible:()=>Unit = ()=>{}): Unit =
//    dom.document.getElementById(title).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
//      .onclick = {(e: MouseEvent) => if(!isVisible) visible() else invisible()}
//
//
//  def get: A
//
//  /**
//   * Executed once at creation time, to append the content to the inside of this box
//   * @param div Placeholder that will receive the "append" with the content of the box
//   * @param visible is true when this box is initially visible (i.e., expanded).
//   */
//  def init(div: Block, visible: Boolean): Unit
//
//  /**
//   * Block of code that should read the dependencies and:
//   *  - update its output value, and
//   *  - produce side-effects (e.g., redraw a diagram)
//   */
//  def update(): Unit
//
//  /** Adds code that is executed everyting the text of the title is clicked.
//   * The code is executed *before* collapsing/expanding. */
//  def whenClickTitle(update: ()=>Unit): Unit =
//    dom.document.getElementById(title).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
//      .onclick = {(_: MouseEvent) => update()}
//
//}
//
//object Box {
//  type Block = DomElem //Selection[dom.EventTarget]
//
//  def downloadSvg(block: Block): Unit = {
//    val svg = block.append("svg")
//      .attr("xmlns","http://www.w3.org/2000/svg")
//      .attr("width","20")
//      .attr("height","20")
//      .attr("viewBox","0 0 24 24")
//      .attr("class", "svgIcon")
//    svg.style("margin","-3pt -2pt 0pt")
//    //svg.style("fill","#505050")
//    svg.append("path")
//      .attr("d","M0 0h24v24H0z")
//      .attr("fill","none")
//    svg.append("path")
//      .attr("d","M19.35 10.04C18.67 6.59 15.64 4 12 4 9.11 4 6.6 5.64 5.35 8.04 2.34 8.36 0 10.91 0 14c0 3.31 2.69 6 6 6h13c2.76 0 5-2.24 5-5 0-2.64-2.05-4.78-4.65-4.96zM17 13l-5 5-5-5h3V9h4v4h3z")
//
//  }
//
//  /**
//   * Default function that catches exceptions and produces an error message based on the type of exception.
//   * @param errorBox is the placeholder where the exception will be appended to.
//   * @return the function to be placed at a catch point.
//   */
//  def checkExceptions(errorBox: OutputArea, source:String = ""): PartialFunction[Throwable,Unit] = {
//    val by = if (source.nonEmpty) s" by $source" else source
//    val f: PartialFunction[Throwable,Unit] = {
//      // type error
//      case e: JavaScriptException => {
//        //      val sw = new StringWriter
//        //      e.printStackTrace(new PrintWriter(sw))
//        //      errorBox.error(/*Show(result)+ */ "JavaScript error : " + e + " - " + e.getClass + "\n" + sw.toString )
//        errorBox.error(/*Show(result)+ */ s"JavaScript error$by: " + e + " - " + e.getClass)
//      }
//      //            instanceInfo.append("p").text("-")
//      case e: java.lang.AssertionError => errorBox.error(e.getMessage)
//      case e: RuntimeException => errorBox.error(s"Runtime error$by: " + e)
//      case e: Throwable => errorBox.error(s"Error$by: " + e + " - " + e.getClass +"\n ### "+e.getStackTrace.mkString("\n - "))
//    }
//    f
//  }
//
//}