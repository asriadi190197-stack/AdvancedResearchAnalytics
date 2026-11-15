/* v1.2.12: adjust DT widths after hidden Bootstrap tabs / accordions become visible. */
(function () {
  "use strict";
  if (typeof document === "undefined") return;
  var scheduled = false;
  function adjustVisibleTables() {
    scheduled = false;
    var $ = window.jQuery;
    if (!$ || !$.fn || !$.fn.dataTable) return;
    try {
      $.fn.dataTable.tables({visible: true, api: true}).columns.adjust();
    } catch (error) {
      if (window.console) console.warn("Penyesuaian tabel:", error);
    }
  }
  function scheduleAdjust() {
    if (scheduled) return;
    scheduled = true;
    window.requestAnimationFrame(adjustVisibleTables);
  }
  document.addEventListener("shown.bs.tab", scheduleAdjust);
  document.addEventListener("shown.bs.collapse", scheduleAdjust);
  document.addEventListener("shown.bs.offcanvas", scheduleAdjust);
  document.addEventListener("shown.bs.modal", scheduleAdjust);
  window.addEventListener("resize", scheduleAdjust);
  if (window.jQuery) {
    window.jQuery(document).on("shown.bs.tab shown.bs.collapse shown.bs.offcanvas shown.bs.modal", scheduleAdjust);
  }
})();
