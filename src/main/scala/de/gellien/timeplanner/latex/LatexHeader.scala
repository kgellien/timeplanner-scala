package de.gellien.timeplanner.latex

import scala.collection.mutable.ListBuffer

object LatexHeader {
  val preamble = """
\addtolength{\oddsidemargin}{-1in}
\addtolength{\evensidemargin}{-1in}
\addtolength{\topmargin}{-1in}
\pagestyle{empty}
\parindent 0pt
"""
  def header(conf: LatexConf) = {
    val result = new ListBuffer[String]
    result += "\\documentclass[12pt,a4paper,landscape]{article}"
    // escape does not work in """ """, neither does single backslash; backslash-u as Unicode-escape?!?
    result += "\\usepackage[landscape]{geometry}"
    result += s"\\usepackage[${conf.encoding}]{inputenc}"
    result += "\\usepackage[ngerman]{babel}"
    result += "\\usepackage[T1]{fontenc}"
    result += "\\usepackage{times}"
    result += "\\usepackage{multicol}"
    result += "\\usepackage{amssymb}"
    result += s"\\setlength{\\textheight}{${conf.pageHeight}}"
    result += s"\\setlength{\\textwidth}{${conf.pageWidth}}"
    result += s"\\oddsidemargin  ${conf.oddsidemargin}"
    result += s"\\evensidemargin ${conf.evensidemargin}"
    result += s"\\topmargin      ${conf.topmargin}"
    result += preamble
  }
}
