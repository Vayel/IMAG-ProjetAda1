-- Ce module facilite la création de fichier SVG.

package SVG is
  procedure init;
  procedure header(w, h: Integer);
  procedure footer;
  procedure startPolygon(lineWidth: Float; color: String);
  procedure addPolyPoint(x, y: Float);
  procedure addRelPolyPoint(dx, dy: Float);
  procedure move(x, y: Float);
  procedure endPolygon;
  function get_contents return String;
end SVG;
