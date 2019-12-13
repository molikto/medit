package medit

import java.io.File

import org.lwjgl._
import org.lwjgl.glfw._
import org.lwjgl.opengl._
import org.lwjgl.system._
import java.nio._

import medit.draw.{Canvas, Position, Rect, ShapeStyle, TextMeasure, TextStyle, Typeface}
import medit.editor.Editor
import medit.structure.Language
import org.lwjgl.glfw.Callbacks._
import org.lwjgl.glfw.GLFW._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL32C._
import org.lwjgl.system.MemoryStack._
import org.lwjgl.system.MemoryUtil._
import org.bytedeco.javacpp.Pointer
import org.bytedeco.skia._
import org.bytedeco.skia.global.Skia._
import utils._


object Main {
  def main(args: Array[String]): Unit = {
    new Window()
  }
}

object typefaces {
  def font(a: String, index: Int): sk_typeface_t = sk_typeface_create_from_file(new File(new File("fonts"), a).getPath, index)
  def apply(a: draw.Typeface): sk_typeface_t = a match {
    case Typeface.Mono => Mono
  }
  val Mono = font("Menlo.ttc", 0)
}


class Impl extends draw.Impl {
  override def measure(textStyle: TextStyle, str: String): TextMeasure = {
    TextMeasure(textStyle.size * str.size * 0.6F, textStyle.size * 0.35F, textStyle.size * 1.05F)
  }

}



class Paints() {
  val text : sk_paint_t = sk_paint_new()
  sk_paint_set_antialias(text, true)
  val shape: sk_paint_t = sk_paint_new()
}

class Shapes {
  val rect: sk_rect_t = new sk_rect_t()
}
object Window {
  lazy val glinterface = gr_glinterface_create_native_interface()
  draw.impl = new Impl()
  var paints: Paints = null
  var shapes: Shapes = null
  def init(): Unit = {
    paints = new Paints()
    shapes = new Shapes()
  }
}

class Window extends Canvas {
  import Window._
  private var window = 0L

  var windowSize: (Int, Int) = (840, 1400)
  var frameBufferSize: (Int, Int) = null
  def calculateDp(): Unit = {
    dp = 1f * frameBufferSize._1 / windowSize._1
    println(s"dp is $dp")
  }
  var dp: Float = 1
  // TODO did they need to be initialized?
  var xpos: Double = 0
  var ypos: Double = 0

  var gr_context: gr_context_t = null
  var gr_ffi: gr_gl_framebufferinfo_t = null
  var gr_backend: gr_backendrendertarget_t = null
  var surface: sk_surface_t = null
  var canvas: sk_canvas_t = null

  GLFWErrorCallback.createPrint(System.err).set
  if (!glfwInit) throw new IllegalStateException("Unable to initialize GLFW")
  glfwDefaultWindowHints()
  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE)
  glfwWindowHint(GLFW_RESIZABLE, GLFW_TRUE)
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3)
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2)
  glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE)
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE)
  glfwWindowHint(GLFW_SRGB_CAPABLE, GL_TRUE)
  glfwWindowHint(GLFW_STENCIL_BITS, 0)
  //glfwWindowHint(GLFW_ALPHA_BITS, 0)
  glfwWindowHint(GLFW_DEPTH_BITS, 0)
  window = glfwCreateWindow(windowSize._1, windowSize._2, "medit", NULL, NULL)
  if (window == NULL) throw new RuntimeException("Failed to create the GLFW window")
  // Get the thread stack and push a new frame
  val stack = stackPush
  try {
    val pWidth = stack.mallocInt(1) // int*
    val pHeight = stack.mallocInt(1)
    glfwGetWindowSize(window, pWidth, pHeight)
    windowSize = (pWidth.get(0), pHeight.get(0))
    debug(s"window size $windowSize")
    val vidmode = glfwGetVideoMode(glfwGetPrimaryMonitor)
    glfwSetWindowPos(window, (vidmode.width - pWidth.get(0)) / 2, (vidmode.height - pHeight.get(0)) / 2)
    glfwGetFramebufferSize(window, pWidth, pHeight)
    frameBufferSize = (pWidth.get(0), pHeight.get(0))
    debug(s"frame buffer size $frameBufferSize")
    calculateDp()
  } finally {
    if (stack != null) stack.close()
  }
  glfwSetWindowSizeCallback(window, (_, width, height) => {
    windowSize = (width, height)
    debug(s"window size changed $windowSize")
    calculateDp()
  })
  glfwSetFramebufferSizeCallback(window, (_, width, height) => {
    frameBufferSize = (width, height)
    debug(s"frame buffer size changed $frameBufferSize")
    calculateDp()
    glfwMakeContextCurrent(window)
    glViewport(0, 0, width, height)
    destroySurface()
    createSurface()
    render()
  })
  // https://www.glfw.org/docs/latest/input_guide.html
  glfwSetCursorPosCallback(window, (_, x, y) =>{
    xpos = x
    ypos = y
  })
  glfwSetCharModsCallback(window, (window: Long, codepoint: Int, mods: Int) => {
    editor.onChar(codepoint, mods)
  })
  glfwSetKeyCallback(window, (window: Long, key: Int, scancode: Int, action: Int, mods: Int) => {
    editor.onKey(key, action, mods)
  })
  glfwSetMouseButtonCallback(window, (_: Long, button: Int, action: Int, mods: Int) => {
    editor.onMouse(button, action, mods, xpos, ypos)
  })
  // TODO scrolling is not frame synced?
  glfwSetScrollCallback(window, (_: Long, xoffset: Double, yoffset: Double) => {
    editor.onScroll(xoffset, yoffset)
  })
  // TODO resizing window will cause drawing to turn black
  glfwMakeContextCurrent(window)
  GL.createCapabilities
  glEnable(GL_BLEND)
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  glEnable(GL_FRAMEBUFFER_SRGB)
  // GrContextOptions options;
  //options.fRequireDecodeDisableForSRGB = false; //was removed?
  gr_context = gr_context_make_gl(Window.glinterface)
  def destroySurface(): Unit = {
    sk_surface_unref(surface)
    gr_backendrendertarget_delete(gr_backend)
    gr_ffi.deallocate()
  }

  def createSurface(): Unit = {
    gr_ffi = new gr_gl_framebufferinfo_t()
    gr_ffi.fFBOID(0)
    gr_ffi.fFormat(GL_RGBA8)
    val colorType = RGBA_8888_SK_COLORTYPE
    //    if (kRGBA_8888_GrPixelConfig == kSkia8888_GrPixelConfig)
    //      kRGBA_8888_SkColorType;
    //      else kBGRA_8888_SkColorType;
    gr_backend = gr_backendrendertarget_new_gl(frameBufferSize._1, frameBufferSize._2, 0, 0, gr_ffi)
    surface = sk_surface_new_backend_render_target(
      gr_context,
      gr_backend,
      BOTTOM_LEFT_GR_SURFACE_ORIGIN,
      colorType,
      null,
      null
    )
    canvas= sk_surface_get_canvas(surface)
  }
  createSurface()
  glfwSwapInterval(1)
  glfwShowWindow(window)
  val rgb = 43f / 255
  glClearColor(rgb, rgb, rgb, 1.0f)
  glViewport(0, 0, frameBufferSize._1, frameBufferSize._2)
  init()

  editor = new Editor(
    Language.parse(utils.read("language-meta.json")),
    ujson.read(utils.read("language-meta.json")),
    a => utils.save(a, "language-meta.json"))
//  editor = new Editor(
//    Language.parse(utils.read("language-meta.json")),
//    ujson.read(utils.read("language-mlang.json")),
//    a => utils.save(a, "language-mlang.json"))
  while (!glfwWindowShouldClose(window)) {
    render()
    gr_context_flush(gr_context)
    glfwSwapBuffers(window)
    glfwWaitEvents()
    // glfwPollEvents()
  }
  destroySurface()
  gr_context_release_resources_and_abandon_context(gr_context)
  // Free the window callbacks and destroy the window
  glfwFreeCallbacks(window)
  glfwDestroyWindow(window)
  // Terminate GLFW and free the error callback
  glfwTerminate()
  glfwSetErrorCallback(null).free()

  var editor: Editor = null



  override def draw(text: String, style: TextStyle, left: Float, top: Float): Unit = {
    val paint = paints.text
    sk_paint_set_color(paint, style.color)
    sk_paint_set_textsize(paint, style.size * dp)
    sk_paint_set_typeface(paint, typefaces(style.typeface))
    sk_canvas_draw_text(canvas, text, text.size, left * dp, top * dp, paint)
  }


  override def draw(rect: Rect, style: ShapeStyle): Unit = {
    val paint = paints.shape
    sk_paint_set_stroke_width(paint, 1 * dp)
    sk_paint_set_color(paint, style.color)
    val r = shapes.rect
    r.left(rect.left * dp)
    r.top(rect.top * dp)
    r.bottom((rect.top + rect.height) * dp)
    r.right((rect.left + rect.width) * dp)
    sk_canvas_draw_rect(canvas, r, paint)
  }

  override def save(): Unit = {
    sk_canvas_save(canvas)
  }

  override def restore(): Unit = {
    sk_canvas_restore(canvas)
  }

  override def translate(x: Float, y: Float): Unit = {
    sk_canvas_translate(canvas, x * dp, y * dp)
  }

  def render(): Unit = {
    glClear(GL_COLOR_BUFFER_BIT)
    editor.render(this, windowSize._1, windowSize._2)
  }

  def renderTest() = {
    val fill = sk_paint_new()
    sk_paint_set_color(fill, 0xFF0000FF)
    sk_canvas_draw_paint(canvas, fill)
    sk_paint_set_color(fill, 0xFF00FFFF)
    val rect = new sk_rect_t
    rect.left(100.0f)
    rect.top(100.0f)
    rect.right(540.0f)
    rect.bottom(380.0f)
    sk_canvas_draw_rect(canvas, rect, fill)
    val stroke = sk_paint_new()
    sk_paint_set_color(stroke, 0xFFFF0000)
    sk_paint_set_antialias(stroke, true)
    sk_paint_set_style(stroke, STROKE_SK_PAINT_STYLE)
    sk_paint_set_stroke_width(stroke, 5.0f)
    val path = sk_path_new
    sk_path_move_to(path, 50.0f, 50.0f)
    sk_path_line_to(path, 590.0f, 50.0f)
    sk_path_cubic_to(path, -490.0f, 50.0f, 1130.0f, 430.0f, 50.0f, 430.0f)
    sk_path_line_to(path, 590.0f, 430.0f)
    sk_canvas_draw_path(canvas, path, stroke)
    sk_paint_set_color(fill, 0x8000FF00)
    val rect2 = new sk_rect_t
    rect2.left(120.0f)
    rect2.top(120.0f)
    rect2.right(520.0f)
    rect2.bottom(360.0f)
    sk_canvas_draw_oval(canvas, rect2, fill)
    sk_path_delete(path)
    sk_paint_delete(stroke)
    sk_paint_delete(fill)
  }
}