-- Ce module facilite la création de fichier SVG.

package SVG is
	function Line(x1, y1, x2, y2: Float) return String;
	function Header(w, h: Integer) return String;
	function Footer return String;
end SVG;
