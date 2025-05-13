document.addEventListener('DOMContentLoaded', function() {
  var toc = document.getElementById('TOC');

  // Mostrar TOC cuando el cursor está cerca del borde izquierdo
  document.addEventListener('mousemove', function(e) {
    if (e.clientX < 250) {
      toc.classList.add('visible');
    } else {
      toc.classList.remove('visible');
    }
  });
});
