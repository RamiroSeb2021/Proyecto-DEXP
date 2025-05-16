# control_bienvenida.R
# Aquí definimos la función que muestra la modal de bienvenida
# Aquí van los nombres en bullets #
show_bienvenida_modal <- function() {
  showModal(
    modalDialog(
      size      = "l",
      easyClose = FALSE,
      fade      = FALSE,
      # Contenedor con fondo GIF
      HTML("
        <div style=\"
          background: url('analizar.gif') center center / cover no-repeat;
          width:100%; height:80vh;
          display:flex; align-items:center; justify-content:center;
        \">
          <div style=\"
            background: rgba(255,255,255,0.9);
            padding: 40px; border-radius: 15px;
            max-width:600px; text-align:center;
            box-shadow:0 0 30px rgba(0,0,0,0.3);
          \">
            <img src='1361b70b-cd7c-4150-aa30-e3f659f15fba.png' style='width:120px;margin-bottom:20px;'/>
            <h1>Bienvenido a la App de Diseño de Experimentos</h1>
            <p>Explora datos, modelos experimentales y calcula tamaños muestrales.</p>
            <p><strong>Estudiantes:</strong> Juan Sebastián Ramírez Ayala, Diana Catalina Hernández Rojas, Yan Carlos Moreno Guerra</p>
            <p><strong>Profesor:</strong> Wilmer Dario Pineda Rios</p>


          </div>
        </div>
      "),
      footer = actionButton("ingresar", "Ingresar", class = "btn btn-success btn-lg")
    )
  )
}

