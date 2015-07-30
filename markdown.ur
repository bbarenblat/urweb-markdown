(* © 2004 John Gruber <http://daringfireball.net/projects/markdown/>
© 2015 Benjamin Barenblat

Licensed under the Apache License, Version 2.0 (the "License"); you may not
use this file except in compliance with the License.  You may obtain a copy
of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied.  See the License for the
specific language governing permissions and limitations under the License. *)

fun lines (text : string) : list string =
  case text of
      "" => []
    | s => case String.split s #"\n" of
               None => s :: []
             | Some (before, after) => before :: lines after

fun unlines (lines : list string) : string =
  List.foldr (fn x y => x ^ "\n" ^ y) "" lines

fun spaces n =
  case n of
      0 => ""
    | x => spaces (n - 1) ^ " "

(* Standardize line endings. *)
fun standardize_line_endings text =
  Regex.replace "\r" "\n"  (* Mac to Unix *)
    (Regex.replace "\r\n" "\n"  (* DOS to Unix *)
       text)

(* Make sure the text ends with a couple of newlines. *)
fun ensure_ends_with_newlines text = text ^ "\n\n"

(* Convert tabs to spaces. *)
fun detab text =
  let
    fun detab_line line =
      case String.split line #"\t" of
          None => line
        | Some (before, after) =>
          detab_line (before ^ spaces (8 - String.length before % 8) ^ after)
  in
    unlines (List.mp detab_line (lines text))
  end

(* Strip any lines consisting of only spaces and tabs.  This makes subsequent
regexes easier to write, because we can match consecutive blank lines with
/\n+/ instead of something contorted like /[ \t]*\n+/. *)
fun strip_blank text =
  Regex.replace "(^|\n) +($|\n)" "\n" text

fun do_headers text =
  (* TODO(bbaren): run_span_gamut on the inner data of the header tags. *)
  let
    (* As of 30 July 2015, the compiler generates incorrect C if I don't
    specify 'text' as a parameter and instead partially apply
    'Regex.replace'. *)
    fun atx_header level text =
      let
        val level_string = show level
      in
        Regex.replace ("(?:^|\n)\#{" ^ level_string ^ "} *(.+?) *\#*\n+")
                      ("<h" ^ level_string ^ ">$1</h" ^ level_string ^ ">\n\n")
                      text
      end
  in
    (atx_header 1
       (atx_header 2
          (atx_header 3
             (atx_header 4
                (atx_header 5
                   (atx_header 6
                      (Regex.replace "(?:^|\n)(.+) *\n-+ *\n+" "<h2>$1</h2>\n\n"
                         (Regex.replace "(?:^|\n)(.+) *\n=+ *\n+"
                                        "<h1>$1</h1>\n\n"
                                        text))))))))
  end

fun run_block_gamut text =
  do_headers text

fun render markdown =
  run_block_gamut
    (strip_blank
       (detab
         (ensure_ends_with_newlines
           (standardize_line_endings markdown))))
