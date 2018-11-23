package org.scalatra.launcher

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder}
import org.scalatra.{ScalatraBase, ScalatraServlet}

/**
 * Launches a Scalatra application using embedded Jetty.
 */
class ScalatraLauncher(app: ScalatraBase) {

  def start(port: Int = 8080): Unit = {
    System.setProperty("org.eclipse.jetty.util.log.class", "org.eclipse.jetty.util.log.StdErrLog")
    System.setProperty("org.eclipse.jetty.LEVEL", "INFO")

    val server = new Server(port)
    //server.setAttribute("org.eclipse.jetty.server.Request.maxFormContentSize", "1")

    val handler = new ServletHandler
    handler.addServletWithMapping(new ServletHolder(new ScalatraServlet(app)), "/*")
    server.setHandler(handler)

    server.start
  }

}

/**
 * Makes a Scalatra application object executable.
 * {{{
 * object HelloApp extends ScalatraApp {
 *   get("hello"){
 *     Ok("Hello World!")
 *   }
 * }
 * }}}
 */
abstract class ScalatraApp extends ScalatraBase with App {
  new ScalatraLauncher(this).start()
}