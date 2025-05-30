# custom_styles.R

custom_css <- "
/* — HEADER Y LOGO — */
.skin-green .main-header .navbar {
  background-color: #14532d !important;
}
.skin-green .main-header .logo {
  background-color: #218838 !important;
  color: #fff !important;
}

/* — SIDEBAR — */
.skin-green .main-sidebar {
  background-color: #1e7e34 !important;
}
.skin-green .sidebar-menu > li.active > a,
.skin-green .sidebar-menu > li > a:hover {
  background-color: #19692c !important;
  color: #fff !important;
}

/* — SUBMENU — */
.skin-green .sidebar-menu .treeview-menu {
  background-color: #14532d !important;
}
.skin-green .sidebar-menu .treeview-menu > li > a {
  color: #c8e6c9 !important;
}
.skin-green .sidebar-menu .treeview-menu > li > a:hover,
.skin-green .sidebar-menu .treeview-menu > li.active > a {
  background-color: #19692c !important;
  color: #fff !important;
}

/* — BOXES PRIMARIAS (verde claro) — */
.skin-green .box.box-solid.box-primary > .box-header {
  background: #28a745 !important;
  border-bottom-color: #218838 !important;
}

/* — BOX INFO (Resultados) — */
.skin-green .box-info .box-header,
.skin-green .box.box-solid.box-info .box-header {
  background-color: #14532d !important;
  border-bottom-color: #11432a !important;
}
.skin-green .box-info .box-header .box-title,
.skin-green .box.box-solid.box-info .box-header .box-title {
  color: #fff !important;
}
.fixed-logo {
  position: fixed;
  bottom: 10px;
  right: 10px;
  width: 80px;
  z-index: 1000;

  /* esto elimina cualquier borde o fondo que el elemento pudiera tener */
  border: none !important;
  background: transparent !important;
}
/* 1) Quitar todo borde de cada <li> y de cada <a> dentro de las pestañas */
.skin-green .nav-tabs-custom > .nav-tabs > li,
.skin-green .nav-tabs-custom > .nav-tabs > li > a {
  border: none !important;
}

/* 2) Pintar sólo el borde superior de la pestaña activa */
.skin-green .nav-tabs-custom > .nav-tabs > li.active {
  border-top: 3px solid #14532d !important;
}

/* 3) Ajustar el <a> de la pestaña activa para que no meta ningún otro borde */
.skin-green .nav-tabs-custom > .nav-tabs > li.active > a {
  background-color: #14532d !important;
  color: #ffffff !important;
}

/* ==== TOOLTIP A LA DERECHA ==== */
/* --- Tooltip a la derecha: clase .tooltip-right --- */

/* 1) Contenedor relativo que envuelve icono y texto */
.tooltip-right {
  position: relative;
  display: inline-block;
  cursor: help;
}

/* 2) El contenido del tooltip, inicialmente oculto */
.tooltip-right .tooltip-right-content {
  position: absolute;
  top: 50%;
  left: 100%;
  transform: translateY(-50%);
  margin-left: 8px;

  padding: 8px 12px;
  background: #3498db;
  color: #fff;
  border-radius: 4px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.2);

  text-align: center;

  white-space: normal;
  word-wrap: break-word;
  /* 1) Aumenta el ancho máximo */
  max-width: 350px !important;
  /* 2) (Opcional) Fija un ancho mínimo para que nunca se encoja demasiado */
  min-width: 200px !important;

  visibility: hidden;
  opacity: 0;
  transition: opacity 0.2s ease-in-out;
  z-index: 10000;
}

/* 3) Al pasar el ratón, lo mostramos */
.tooltip-right:hover .tooltip-right-content {
  visibility: visible;
  opacity: 1;
}
/* — Separa el icono del texto y lo pinta de azul — */
.tooltip-right {
  margin-left: 8px !important;    /* separación del texto */
  color: #3498db !important;       /* color azul para el icono */
}

/*Sroll contenido*/
.content-wrapper {
  overflow-y: auto;
  height: calc(100vh - 50px); /* 50px es la altura aproximada del header */
  padding-bottom: 20px;
}

"
