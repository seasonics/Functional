structure Derivation : DERIVATION = struct

  fun draw (sourceCode, optFilename) = let
    (* note: you are free to restructure this function however 
     * you like, but continue to respect the signature 
     *)
    val toks = Lex.lex sourceCode
    val ast  = Parse.parse toks
	local 
		fun dtree(e) =
			case e
				of AST.True => (Ty.Bool,"true : Bool")
				| AST.False => (Ty.Bool,"false: Bool")
				| AST.Zero =>  (Ty.Nat,"0: Nat")
				| AST.Succ(e1) => let 
									val(x,y) = dtree(e1)
								 in 
									case x
										of Ty.Nat => (Ty.Nat, "<table><td>" ^ y ^ "</td><tr><td class='horizontal-line'></tr><tr><td>Succ ("^AST.tos e1^") :Nat </td></tr></table>")
										| _ =>(Ty.Invalid, "<table><td>" ^ y ^ "</td><tr><td class='horizontal-line'></tr><tr><td><font color='red'>Succ ("^AST.tos e1^") :X</font></td></tr></table>")
								 end
				| AST.Pred(e1) => let 
									val(x,y) = dtree(e1)
								 in 
									case x
										of Ty.Nat => (Ty.Nat, "<table><td>" ^ y ^ "</td><tr><td class='horizontal-line'></tr><tr><td>Pred ("^AST.tos e1^") :Nat </td></tr></table>")
										| _ =>(Ty.Invalid, "<table><td>" ^ y ^ "</td><tr><td class='horizontal-line'></tr><tr><td><font color='red'>Pred ("^AST.tos e1^") :X</font></td></tr></table>")
								 end
				| AST.IsZero(e1) => let 
										val(x,y) = dtree(e1)
								    in 
										case x
											of Ty.Nat => (Ty.Nat, "<table><td>" ^ y ^ "</td><tr><td class='horizontal-line'></tr><tr><td>IsZero ("^AST.tos e1^") :Nat </td></tr></table>")
											| _ =>(Ty.Invalid, "<table><td>" ^ y ^ "</td><tr><td class='horizontal-line'></tr><tr><td><font color='red'>IsZero ("^AST.tos e1^") :X</font></td></tr></table>")
									end
				| AST.If(e1,e2,e3) => let 
										val(x,y) = dtree(e1)
										val(x2,y2) = dtree(e2)
										val(x3,y3) = dtree(e3)
									  in 
										case x
											of Ty.Bool => if Ty.eq (x2,x3)
														  then (x2, "<table><td>" ^ y ^ "</td><td>" ^ y2 ^ "</td><td>" ^ y3 ^ "</td><tr><td class='horizontal-line' colspan='3'></tr><tr><td>If(("^AST.tos e1^")</td><td> then ("^AST.tos e2^") </td><td>else ("^AST.tos e3^") :"^ Ty.tos x2^"</td></tr></table>")
														  else (Ty.Invalid, "<table><td>" ^ y ^ "</td><td>" ^ y2 ^ "</td><td>" ^ y3 ^ "</td><tr><td class='horizontal-line' colspan='3'></tr><tr class = 'red'><td>If(("^AST.tos e1^")</td><td> then ("^AST.tos e2^")</td><td> else ("^AST.tos e3^") : X</td></tr></table>")
											| _ =>(Ty.Invalid, "<table><td>" ^ y ^ "</td><td>" ^ y2 ^ "</td><td>" ^ y3 ^ "</td><tr><td class='horizontal-line' colspan='3'></tr><tr class = 'red'><td>If(("^AST.tos e1^")</td><td> then ("^AST.tos e2^")</td><td> else ("^AST.tos e3^") : X</td></tr></table>")
									  end
	in
	val table = #2(dtree(ast))
	end

    val optOutfile = 
     (case optFilename
       of NONE   => NONE
	| SOME f => SOME (TextIO.openOut f))
    val println =
     (case optOutfile
       of NONE     => (fn s => (TextIO.print s; 
				TextIO.print "\n"))
	| SOME out => (fn s => (TextIO.output (out, s);
				TextIO.output (out, "\n")))
     (* end case *))
    val _ = List.app println 
		     ["<html>",
		      "<head>",
		      "<style>",
		      "th { text-align : right; }",
		      "table {text-align : center; }",
		      "tr.red {color:red;}",
		      ".horizontal-line {background-color: black}",
		      "</style>",
		      "</head>",
		      "<body>",
		      "<p>"^ table ^"</p>",
		      "<p>",
		      "<table>",
		      "<tr><th>code</th><td><code>" ^ sourceCode ^ "</code></td></tr>",
		      "<tr><th>parsed code</th><td><code>" ^ AST.tos ast ^ "</code></td></tr>",
		      "<tr><th>unparsed</th><td><code>" ^ AST.unparse ast ^ "</code></td></tr>",
		      "</table>",
		      "</p>",
		      "</body>",
		      "</html>"]

    in
      case optOutfile
       of SOME out => TextIO.closeOut out
	| NONE     => ()
    end

end
