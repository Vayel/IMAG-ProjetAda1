-- Ce module facilite la création de fichier SVG.

package SVG is
  type Point is record
    x: Float;
    y: Float;
  end record;

  type Points is array (Integer range <>) of Point;

  procedure init;
	procedure header(w, h: Integer);
	procedure footer;
	procedure polygon(pts: Points; width: Float; color: String);
  function get_contents return String;
end SVG;
