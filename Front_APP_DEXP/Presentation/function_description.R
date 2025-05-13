# Funcion sin costo -------------------------------------------------------

desc_sinCosto <- "
            .mi-tooltip {
              position: relative;
              display: inline-block;
              cursor: pointer;
            }
            .mi-tooltip .texto-tooltip {
              visibility: hidden;
              width: 160px;
              background-color: #3498db;
              color: white;
              text-align: center;
              border-radius: 6px;
              padding: 8px;
              position: absolute;
              z-index: 1000;
              bottom: 125%;
              left: 50%;
              transform: translateX(-50%);
              opacity: 0;
              transition: opacity 0.3s;
              font-size: 14px;
            }
            .mi-tooltip:hover .texto-tooltip {
              visibility: visible;
              opacity: 1;
            }
            .mi-tooltip .texto-tooltip::after {
              content: '';
              position: absolute;
              top: 100%;
              left: 50%;
              margin-left: -5px;
              border-width: 5px;
              border-style: solid;
              border-color: #3498db transparent transparent transparent;
            }
          "
  

# Funcion conCosto --------------------------------------------------------

desc_conCosto <- "
                  .mi-tooltip {
                    position: relative;
                    display: inline-block;
                    cursor: pointer;
                  }
                  .mi-tooltip .texto-tooltip {
                    visibility: hidden;
                    width: 160px;
                    background-color: #3498db;
                    color: white;
                    text-align: center;
                    border-radius: 6px;
                    padding: 8px;
                    position: absolute;
                    z-index: 1000;
                    bottom: 125%;
                    left: 50%;
                    transform: translateX(-50%);
                    opacity: 0;
                    transition: opacity 0.3s;
                    font-size: 14px;
                  }
                  .mi-tooltip:hover .texto-tooltip {
                    visibility: visible;
                    opacity: 1;
                  }
                  .mi-tooltip .texto-tooltip::after {
                    content: '';
                    position: absolute;
                    top: 100%;
                    left: 50%;
                    margin-left: -5px;
                    border-width: 5px;
                    border-style: solid;
                    border-color: #3498db transparent transparent transparent;
                  }
                "