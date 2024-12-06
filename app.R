library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotrix)
library(DT)  # Untuk menampilkan tabel interaktif
library(stringr)
library(broom.helpers)
library(car)
library(caret)
library(ggplot2)
library(gridExtra)
library(plotly)
library(shinyBS)

# Read Data
vacation_data <- read.csv('https://raw.githubusercontent.com/erdanisaaghnia/evd/refs/heads/main/data_mod.csv')
library(dplyr)

# Mengubah tipe data sesuai dengan skala yang diinginkan
vacation_data <- vacation_data %>%
  mutate(
    # Variabel numerik
    Umur = as.numeric(Umur),
    Penghasilan = as.numeric(Penghasilan),
    Frekuensi_Liburan = as.numeric(Frekuensi_Liburan),
    Anggaran_Liburan = as.numeric(Anggaran_Liburan),
    Jarak_ke_Gunung = as.numeric(Jarak_ke_Gunung),
    Jarak_ke_Pantai = as.numeric(Jarak_ke_Pantai),
    
    # Variabel kategorik (faktor)
    Jenis_Kelamin = as.factor(Jenis_Kelamin),
    Pendidikan = as.factor(Pendidikan),
    Preferensi_Aktivitas = as.factor(Preferensi_Aktivitas),
    Lokasi = as.factor(Lokasi),
    Musim_Favorit = as.factor(Musim_Favorit),
    
    # Variabel biner (0/1)
    Hewan_Peliharaan = factor(Hewan_Peliharaan),
    Sensitivitas_Lingkungan = factor(Sensitivitas_Lingkungan),
    Preferensi_Liburan = factor(Preferensi_Liburan)
  )


# UI
ui <- dashboardPage(
  dashboardHeader(title = tags$span(
    "Preferensi Liburan", 
    style = "
        font-weight: bold;
        font-size: 20px;
        color: white;
        #font-style: italic; /* Mengatur teks miring */
        font-family: 'Comic Sans MS', cursive, sans-serif; /* Menggunakan font berbeda */
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                     -2px -2px 4px rgba(0, 0, 0, 0.5), 
                     2px -2px 4px rgba(0, 0, 0, 0.5), 
                     -2px 2px 4px rgba(0, 0, 0, 0.5);
      ")
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Halaman Utama", tabName = "logistic_material", 
               icon = icon("book", lib = "font-awesome", class = "fa-1.5x")),
      
      menuItem("Overview", tabName = "overview", 
               icon = icon("chart-pie", lib = "font-awesome", class = "fa-1.5x")),
      
      selectInput("season_filter", "Filter Berdasarkan Musim Favorit:",
                  choices = c("Semua", "Panas", "Dingin", "Semi", "Gugur"), selected = "Semua"),
      
      sliderInput("age_filter", "Filter Berdasarkan Rentang Umur:",
                  min = min(vacation_data$Umur), max = max(vacation_data$Umur),
                  value = c(min(vacation_data$Umur), max(vacation_data$Umur)), step = 1),
      
      selectInput("location_filter", "Filter berdasarkan Lokasi Tinggal:",
                  choices = c("Semua", "Perkotaan", "Pinggiran Kota", "Pedesaan"), selected = "Semua"),
      
      selectInput("color_scheme", "Pilih Skema Warna:", 
                  choices = c("Set1", "Set2", "Set3", "Paired", "Dark2"), selected = "Paired"),
      
      menuItem("EDA", tabName = "eda", 
               icon = icon("bar-chart", lib = "font-awesome", class = "fa-1.5x")),
      
      menuItem("Regresi Logistik", tabName = "logistic_regression", 
               icon = icon("line-chart", lib = "font-awesome", class = "fa-1.5x"))
    )
    
  ),
  
  dashboardBody(
    # Tambahkan Font Awesome untuk ikon
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    
    # Tambahkan CSS langsung di bagian `tags$head`
    tags$head(
      tags$style(HTML(
        "
        .content-wrapper {
          background-image: url(bg.jpg);
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          min-height: 100%;
          padding-bottom: 20px;
          position: relative;
        }
        
          /* Membuat lapisan transparan di atas gambar */
        .content-wrapper::after {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background-color: rgba(255, 255, 255, 0.2); /* transparansi 40% */
          z-index: 0; /* Menurunkan z-index agar elemen lainnya dapat diakses */
          pointer-events: none; /* Agar lapisan transparan tidak menghalangi klik */
        }
        
        
        /* CSS untuk font sidebar*/
      .main-sidebar .sidebar-menu a {
        font-size: 20px;
        font-family: 'Comic Sans MS', cursive, sans-serif;
        font-weight: bold;
        color: #FFFFFF !important;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3);
      }
      .main-sidebar .sidebar-menu a:hover {
        color: #FFD700 !important;
        text-shadow: 2px 2px 6px rgba(255, 215, 0, 0.8);
      }
      .main-sidebar .sidebar .sidebar-menu {
        padding-top: 20px;
      }
        
        /* Adjust the sidebar width */
        .main-sidebar {
          width: 250px;
        }
        
        .content-wrapper {
          margin-left: 250px;
          z-index: 2;
        }
        /* Make sure text and plots stay on top of the background */
        .box {
          z-index: 3;
          position: relative;
        }
        
        /* Box styling with 3D effect */
        .box {
          z-index: 3;
          position: relative;
          border-radius: 15px; /* Rounded corners */
          box-shadow: 0px 8px 16px rgba(0, 0, 0, 0.15), 0px 3px 6px rgba(0, 0, 0, 0.75); /* 3D shadow effect */
          background-color: #ffffff; /* White background */
          border: 1px solid #ddd; /* Light border */
          transition: all 0.3s ease; /* Smooth transition for hover effect */
        }
        
        .box:hover {
          box-shadow: 0px 16px 32px rgba(0, 0, 0, 0.2), 0px 6px 12px rgba(0, 0, 0, 0.15); /* Shadow intensifies on hover */
          transform: translateY(-4px); /* Slight lift effect */
        }

        .box .box-header {
          border-top-left-radius: 15px;
          border-top-right-radius: 15px;
          background-color: #007bff; /* Blue header background */
          color: white; /* White text */
        }
        
        .box .box-body {
          padding: 15px;
        }
        
        
        /* Animasi saat buka app*/
        .box {
        opacity: 0;
        animation: fadeIn 1s forwards;
        }
        
        @keyframes fadeIn {
        0% { opacity: 0; }
        100% { opacity: 1; }
        }
        
      /* Menambah Animasi Bayangan dan Hover di Tombol*/
      .btn-3d {
      background-color: #4CAF50;
      border: none;
      color: white;
      padding: 15px 32px;
      text-align: center;
      font-size: 16px;
      cursor: pointer;
      border-radius: 12px;
      box-shadow: 0 6px 15px rgba(0, 0, 0, 1);
      transition: all 0.3s ease;
      }
      
      .btn-3d:hover {
      box-shadow: 0 12px 30px rgba(0, 0, 0, 0.7);
      transform: translateY(-5px);
      }
      
      /* Efek aktif pada item menu */
        .main-sidebar .sidebar-menu li.active a {
          background-color: #2F4F4F; /* Warna hijau gelap untuk menu yang aktif */
          color: white !important;
        }
        
      /* Membuat Gradasi pada Header Box */
      .skin-blue .box .box-header {
        background: linear-gradient(to right,   #4682B4, #5F9EA0) !important; /* Gradasi dari hijau ke biru */
        color: white !important; /* Mengubah warna teks menjadi putih */
      }
      
      /* Custom Button */
        .custom-button {
        color: black;
        border: none;
        border-radius: 8px;
        padding: 10px 15px;
        font-size: 14px;
        cursor: pointer;
        transition: background-color 0.3s, transform 0.2s;
      }
      .custom-button:hover {
        background-color: #ADD8E6;
        transform: scale(1.05);
      }
      
    
    
   .modern-card {
        font-family: 'Poppins', sans-serif; /* Font modern */
        background: linear-gradient(180deg, #E0FFFF, #F5EDED); /* Gradasi warna */
        border-radius: 12px;
        padding: 18px;
        color: 	#191970; /* Warna tulisan hitam */
        font-size: 20px;
        box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2);
        text-align: center;
   }
      
      .modern-card h3 {
        font-size: 18px;
        margin-bottom: 5px;
      }
      .modern-card span {
        font-size: 25px;
        font-weight: bold;
        color: 	#191970; /* Warna angka */
      }
      .modern-card i {
        font-size: 50px;
        margin-bottom: 10px;
        color: 	#191970; /* Warna ikon */
      }
       .summary-box {
        background-color: #1f4a70;
        color: white;
        padding: 15px;
        margin-top: 15px;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);
      }
      h4 {
        font-weight: bold;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
      }
      .alert-warning {
        background-color: #f8d7da;
        border-color: #f5c6cb;
        color: #721c24;
        padding: 15px;
        margin-top: 20px;
        border-radius: 10px;
      }
      
      
      /* Custom Tombol Jalankan Model dan Muat Data */
      #fit_model, #load_data, #load_data2 {
        background: linear-gradient(145deg, #227B94, #16325B); /* Gradient biru ke ungu */
        color: white;
        border: none;
        border-radius: 10px;
        padding: 12px 14px;
        font-size: 13px;
        font-family: 'Lato', sans-serif; /* Font modern dan elegan */
        font-weight: 600;
        cursor: pointer;
        transition: all 0.4s ease-in-out;
      }
      
      #fit_model:hover, #load_data:hover, #load_data2 {
        background: linear-gradient(145deg, #16325B, #227B94); /* Membalikkan gradient */
        transform: translateY(-6px); /* Efek hover yang halus */
      }
      
      #fit_model:active, #load_data:active, #load_data2:active {
        transform: translateY(2px); /* Sedikit masuk saat diklik */
        box-shadow: 0px 5px 15px rgba(0, 0, 0, 0.1);
      }
      
      
      
      /*PENGATURAN TAB PANEL */
      .nav-pills > li.active > a {
        background-color: #3498db !important;
        color: white;
        border-radius: 20px;
        box-shadow: 0 0 15px rgba(52, 152, 219, 0.6); /* Efek cahaya */
        transition: all 0.3s ease;
      }
    
      .nav-pills > li > a {
        background-color: #ecf0f1;
        color: #2c3e50;
        border-radius: 20px;
        padding: 12px 25px;
        font-weight: bold;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
      }
    
      .nav-pills > li > a:hover {
        background-color: #2980b9;
        color: white;
        box-shadow: 0 0 10px rgba(41, 128, 185, 0.7); /* Efek hover lebih intens */
        transform: scale(1.05);
      }
      
      
      /* Gaya untuk judul panel */
      .panel-title-custom {
        font-family: 'Montserrat', sans-serif;
        font-size: 24px;
        font-weight: bold;
        color: #4A4A4A;
        text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.3);
      }

      /* Mengubah warna kotak panel saat diexpand */
      .panel-collapse.in {
        background-color: white !important;
        border-radius: 10px;
      }

      /* Mengubah warna background heading panel */
      .panel-heading {
        background-color: #FEF9F2 !important;
        border-radius: 10px;
      }


      /* Gaya untuk konten dalam panel yang diexpand */
      .panel-body {
        font-size: 16px;
        color: black;
        line-height: 1.3;
        font-family: 'Frutiger', sans-serif;
      }


        "
      ))
    ),
    
    ################### TAB ITEM : PENGANTAR ################       
    tabItems(
      
      tabItem(
        tabName = "logistic_material",
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              id = "subtabs_pengantar", 
              type = "pills", # Gaya navigasi seperti tombol
              
              # Tab 1: Tentang Dashboard
              tabPanel(
                title = tagList(
                  tags$img(
                    src = "dashboard.png", 
                    style = "height: 35px; width: 35px; margin-right: 5px;"
                  ), 
                  "Dashboard"
                ),
                
                fluidRow(
                  column(
                    width = 8, height='5px',
                    offset=2,
                    wellPanel(
                      style = "
                      background-color: rgba(255, 255, 255, 0);  /* Transparan bening */
                      border-radius: 15px; 
                      box-shadow: none; 
                      border: none;",
                      
                      tags$h1("Jelajahi Preferensi Liburan", 
                              style = "
                    font-family: 'Frutiger', sans-serif; 
                    font-weight: bold; 
                    font-size: 50px;
                    color: white; 
                    -webkit-text-stroke: 2px black;  /* Garis tepi hitam di sekitar huruf */
                    text-shadow: 
                      2px 2px 4px rgba(0, 128, 0, 0.4),   /* Bayangan hijau gelap dengan transparansi lebih rendah */
                      -2px -2px 6px rgba(0, 128, 0, 0.3),  /* Bayangan hijau gelap lebih lembut */
                      0 0 8px rgba(0, 128, 0, 0.5),       /* Bayangan hijau gelap lebih tajam */
                      0 0 12px rgba(0, 128, 0, 0.3),       /* Bayangan hijau gelap lebih halus */
                      0 0 14px rgba(0, 128, 0, 0.4),       /* Bayangan hijau gelap konsisten */
                      4px 4px 6px rgba(0, 0, 128, 0.4),    /* Bayangan biru tua dengan transparansi */
                      -4px -4px 8px rgba(0, 0, 128, 0.3),  /* Bayangan biru tua lebih halus */
                      0 0 10px rgba(0, 0, 128, 0.5);      /* Bayangan biru tua lebih tajam */

                    letter-spacing: 3px;
                    transform: perspective(500px) rotateX(-10deg) rotateY(10deg);
                    margin-bottom: 20px;")
                    )
                  )
                ),
                
                
                fluidRow(
                  style = "gap: 10px; margin: 0;",
                  
                  # Kolom pertama
                  column(
                    width = 4,
                    style = "padding-right: 5px;",
                    wellPanel(
                      style = "
                      background: linear-gradient(135deg, #ffffff, #f0f0f0); 
                      border: 2px solid #dcdcdc; 
                      border-radius: 15px; 
                      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15), inset 0 0 5px rgba(0, 0, 0, 0.1); 
                      padding: 15px;",
                      
                      tags$div(style = "display: flex; align-items: center; justify-content: center; height: 100%;",
                               tags$img(src = "gunung1.jpg", height = "300px",
                                        width = "100%", 
                                        style = "
                          border-radius: 10px; 
                          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);"))
                    )
                  ),
                  
                  # Kolom kedua
                  column(
                    width = 4,
                    style = "padding-left: 5px;",
                    wellPanel(
                      style = "
                      background: linear-gradient(135deg, #ffffff, #f0f0f0); 
                      border: 2px solid #dcdcdc; 
                      border-radius: 15px; 
                      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15), inset 0 0 5px rgba(0, 0, 0, 0.1); 
                      padding: 15px;",
                      
                      tags$div(style = "display: flex; align-items: center; justify-content: center; height: 100%;",
                               tags$img(src = "pantai2.jpg", height = "300px",
                                        width = "100%", 
                                        style = "
                          border-radius: 10px; 
                          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);"))
                    )
                  ),
                  
                  # Kolom ketiga
                  column(
                    width = 4,
                    style = "padding-left: 5px;",
                    wellPanel(
                      style = "
                      background: linear-gradient(135deg, #ffffff, #f0f0f0); 
                      border: 2px solid #dcdcdc; 
                      border-radius: 15px; 
                      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15), inset 0 0 5px rgba(0, 0, 0, 0.1); 
                      padding: 15px;",
                      
                      tags$div(style = "display: flex; align-items: center; justify-content: center; height: 100%;",
                               tags$img(src = "gunung2.jpg", height = "300px",
                                        width = "100%", 
                                        style = "
                          border-radius: 10px; 
                          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);"))
                    )
                  )
                ),
                
                
                fluidRow(
                  column(
                    width = 12,
                    wellPanel(
                      style = "
                background-color: rgba(255, 255, 255, 0);  /* Transparan bening */
                border-radius: 15px; 
                box-shadow: none; 
                border: none; 
                padding: 20px;",  # Menambahkan padding agar teks tidak terlalu dekat dengan tepi panel
                      
                      tags$div(
                        style = "
                  background-color: rgba(255, 255, 255, 0);  /* Transparan bening */
                  border-radius: 15px; 
                  box-shadow: none; 
                  border: none;",
                        
                        tags$p(
                          "Dashboard ini dirancang untuk mengeksplorasi preferensi liburan antara pantai dan pegunungan.
                    Melalui visualisasi dan analisis mendalam, Anda dapat memahami berbagai faktor yang memengaruhi pilihan ini.",
                          style = "
                    font-size: 18px; 
                    color: white; 
                    line-height: 1.8; 
                    font-family: 'Frutiger', sans-serif; 
                    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.6);"
                        ),
                        
                        tags$ul(
                          tags$li(
                            tags$strong(tags$em("Overview")), ": Gambaran umum dari data."
                          ),
                          tags$li(
                            tags$strong(tags$em("Exploratory Data Analysis (EDA)")), ":
                      Anda dapat melakukan eksplorasi data dengan berbagai
                      visualisasi yang menggambarkan hubungan antar variabel."
                          ),
                          tags$li(
                            tags$strong(tags$em("Regresi Logistik")), ": Anda dapat melakukan pemodelan regresi
                      logistik yang diterapkan untuk memahami faktor-faktor yang mempengaruhi preferensi liburan
                       (baik pada dataset dashboard ini maupun data yang Anda unggah)."
                          ),
                          style = "
                    font-size: 18px; 
                    color: white; 
                    line-height: 1.8; 
                    font-family: 'Frutiger', sans-serif; 
                    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.6);"
                        ),
                        
                        
                        
                        
                        tags$br(),
                        
                        tags$h4(
                          "Pengembang:", 
                          style = "
                    margin-top: 20px; 
                    font-family: 'Playfair Display', serif; 
                    font-weight: bold; 
                    font-size: 18px; 
                    color: white; 
                    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.6);"
                        ),
                        
                        tags$p(
                          "Indra Rivaldi Siregar | Erdanisa Aghnia Ilmani | Nabila Tri Amanda",
                          style = "
                    font-size: 14px; color: white;
                    font-family: 'Frutiger', sans-serif;
                    text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.5);"
                        )
                        
                      )
                    )
                    
                  )
                )
              ),
              
              # Tab 2: Tentang Dataset
              tabPanel(
                title = tagList(
                  tags$img(
                    src = "data.png", 
                    style = "height: 35px; width: 35px; margin-right: 5px;"
                  ), 
                  "Dataset"
                ),
                
                fluidRow(
                  tags$br(),
                  
                  column(
                    width = 12,
                    wellPanel(
                      style = "
                      background: linear-gradient(145deg, #789DBC, #FEF9F2, #C9E9D2); /* Gradasi warna */
                      border-radius: 15px; 
                      color: black;
                      border: none;",
                      
                      tags$h4("Tentang Dataset", 
                              style = "
                              font-family: 'Montserrat', sans-serif; 
                              font-size: 30px; 
                              font-weight: bold; 
                              color: black; 
                              text-shadow: 
                                3px 3px 5px rgba(0, 0, 0, 0.4),  /* Bayangan ringan */
                                0 0 15px rgba(0, 0, 0, 0.7),   /* Bayangan yang lebih jauh */
                            "
                      ),
                      
                      tags$br(),
                      
                      
                      tags$p("Dataset ini digunakan untuk menggali preferensi publik antara dua jenis liburan:
                             pegunungan dan pantai. Melalui analisis mendalam terhadap data demografi
                             dan gaya hidup, pengguna dapat menemukan pola unik yang memengaruhi pilihan liburan.",
                             
                             style = "
                            font-size: 16px; 
                            color: black; 
                            line-height: 1.3; 
                            font-family: 'Frutiger', sans-serif;"
                             
                      ),
                      
                      tags$br(),
                      
                      
                      tags$h3("Struktur Dataset:", style = "margin-top: 5px;"),
                      p("Jumlah Data (Baris) : 52444",
                        
                        style = "
                            font-size: 16px; 
                            color: black; 
                            line-height: 1; 
                            font-family: 'Frutiger', sans-serif;"
                        
                        
                      ),
                      p("Jumlah Variabel: 13",
                        
                        style = "
                            font-size: 16px; 
                            color: black; 
                            line-height: 0.6; 
                            font-family: 'Frutiger', sans-serif;"
                        
                        
                      ),
                      
                      tags$br(),
                      
                      tags$h3("Variabel meliputi:", style = "margin-top: 5px;"),
                      
                      tags$ul(
                        tags$li(tags$strong("Usia:"), " Usia individu (numerik)."),
                        tags$li(tags$strong("Jenis Kelamin:"), " Identitas gender individu (kategorikal: laki-laki, perempuan, non-biner)."),
                        tags$li(tags$strong("Penghasilan:"), " Pendapatan tahunan individu (numerik)."),
                        tags$li(tags$strong("Pendidikan:"), " Tingkat pendidikan tertinggi yang dicapai (kategorikal: SMA, sarjana, master, doktor)."),
                        tags$li(tags$strong("Frekuensi Liburan:"), " Jumlah liburan yang diambil per tahun (numerik)."),
                        tags$li(tags$strong("Aktivitas yang Disukai:"), " Aktivitas yang disukai individu saat liburan (kategorikal: hiking, berenang, ski, berjemur)."),
                        tags$li(tags$strong("Anggaran Liburan:"), " Anggaran yang dialokasikan untuk liburan (numerik)."),
                        tags$li(tags$strong("Lokasi:"), " Jenis tempat tinggal (kategorikal: perkotaan, pinggiran kota, pedesaan)."),
                        tags$li(tags$strong("Jarak ke Pegunungan:"), " Jarak dari pegunungan terdekat (numerik, dalam mil)."),
                        tags$li(tags$strong("Jarak ke Pantai:"), " Jarak dari pantai terdekat (numerik, dalam mil)."),
                        tags$li(tags$strong("Musim Favorit:"), " Musim yang disukai untuk liburan (kategorikal: musim panas, musim dingin, musim semi, musim gugur)."),
                        tags$li(tags$strong("Pemilik Hewan Peliharaan:"), " Menunjukkan apakah individu memiliki hewan peliharaan (biner: 0 = Tidak, 1 = Ya)."),
                        tags$li(tags$strong("Sensitivitas Lingkungan:"), " Menunjukkan apakah individu memiliki kekhawatiran terhadap lingkungan (biner: 0 = Tidak, 1 = Ya)."),
                        
                        style = "
                        font-size: 16px; 
                        color: black; 
                        line-height: 1.5; 
                        font-family: 'Frutiger', sans-serif;"
                      )
                      
                      
                    )
                  )
                )
              ),
              
              # Tab 3: Materi Regresi Logistik
              tabPanel(
                title = tagList(
                  tags$img(
                    src = "logistic-regression.png", 
                    style = "height: 35px; width: 35px; margin-right: 5px;"
                  ), 
                  "Metode"
                ),
                
                fluidRow(
                  tags$br(),
                  tags$br(),
                  column(
                    width = 12,
                    bsCollapse(
                      id = "collapsePanels", 
                      open = NULL,  # Tidak membuka panel secara default
                      
                      bsCollapsePanel(
                        title = tags$span("📈 Regresi Logistik", class = "panel-title-custom"),
                        value = "panel1",
                        wellPanel(
                          style = "
            background: linear-gradient(145deg, #789DBC, #FEF9F2, #C9E9D2); /* Gradasi warna */
            border-radius: 15px; 
            color: black;
            border: none;",
                          
                          tags$h4("Tentang Regresi Logistik", 
                                  style = "
                    font-family: 'Montserrat', sans-serif; 
                    font-size: 30px; 
                    font-weight: bold; 
                    color: black; 
                    text-shadow: 
                      3px 3px 5px rgba(0, 0, 0, 0.4),  /* Bayangan ringan */
                      0 0 15px rgba(0, 0, 0, 0.7),   /* Bayangan yang lebih jauh */
                  "
                          ),
                          
                          
                          tags$p("Regresi logistik adalah metode regresi yang digunakan untuk memodelkan hubungan antara satu variabel
                   dependen kategorikal dengan satu atau lebih variabel independen.
                   Biasanya, variabel dependen yang digunakan dalam regresi logistik adalah variabel biner
                   (dua kategori), seperti ya/tidak, sukses/gagal, atau lainnya.",
                                 
                                 style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          ),
                          
                          tags$img(
                            src = "logistik.png", 
                            height = "300px", 
                            width = '400px',
                            style = "display: block; margin: 10px auto; border-radius: 15px; box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2);"
                          ),
                          
                          
                          tags$h5("1. Fungsi Logistik (Sigmoid)", style = "font-size: 20px; font-weight: bold;"),
                          tags$p(
                            "Regresi logistik menggunakan fungsi logistik atau sigmoid, yang mengubah output linear menjadi probabilitas
              dalam rentang antara 0 dan 1. Dalam regresi logistik, fungsi ini membentuk kurva S dan menentukan nilai
              ambang batas (threshold) untuk memutuskan probabilitas 0 atau 1.
              Nilai di atas ambang batas cenderung ke 1, sementara nilai di bawah ambang batas cenderung ke 0.",
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          ),
                          
                          tags$h5("2. Model Regresi Logistik", style = "font-size: 20px; font-weight: bold;"),
                          
                          tags$ul(
                            
                            tags$li(
                              "Dalam regresi logistik, koefisien model menggambarkan perubahan pada log-odds
              untuk setiap unit perubahan pada prediktor. Jika koefisien positif, maka odds (kemungkinan) suatu peristiwa terjadi
              akan meningkat dengan bertambahnya nilai prediktor. Sebaliknya, koefisien negatif menunjukkan bahwa odds suatu peristiwa
              terjadi akan berkurang.",
                              style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                            ),
                            
                            tags$li(
                              "Koefisien dalam model regresi logistik dapat diubah menjadi odds-ratio (OR) dengan menghitung exp(koefisien).",
                              style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                            ),
                            
                            tags$li(
                              "Persamaan log-odds regresi logistik: ",
                              tags$code("log(P(Y=1|X) / (1 - P(Y=1|X))) = b0 + b1*X1 + b2*X2 + ... + bn*Xn"),
                              style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                            ),
                            
                            tags$li(
                              "Untuk mengubah koefisien menjadi odds ratio, gunakan rumus: ",
                              tags$code("OR = exp(b1), OR = exp(b2), ... , OR = exp(bn)"),
                              style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                            )
                          ),
                          
                          
                          tags$h5("3. Asumsi Regresi Logistik", style = "font-size: 20px; font-weight: bold;"),
                          
                          tags$ul(
                            tags$li("Variabel dependen harus bersifat kategorikal (misalnya, dua kelas: 0 atau 1)."),
                            tags$li("Tidak boleh ada multikolinearitas antara variabel independen. Artinya, variabel independen seharusnya tidak saling berkorelasi tinggi."),
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          )
                          
                          
                        )
                      ),
                      
                      bsCollapsePanel(
                        title = tags$span("📊 Log-odds dan Odds-Ratio", class = "panel-title-custom"),
                        value = "panel2",
                        
                        wellPanel(
                          style = "
                background: linear-gradient(145deg, #789DBC, #FEF9F2, #C9E9D2); 
                border-radius: 15px; 
                color: black;
                border: none;",
                          
                          tags$h5(tags$strong("Odds"), style = "font-size: 20px; font-weight: bold;"),
                          p("Odds adalah rasio antara peluang terjadinya suatu kejadian dengan peluang tidak terjadinya kejadian tersebut.", 
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          ),
                          
                          tags$div(
                            style = "font-size: 20px;",
                            withMathJax("$$\\text{Odds} = \\frac{P}{1-P}$$")
                          ),
                          
                          
                          tags$h5(tags$strong("Odds Ratio (OR)", style = "font-size: 20px; font-weight: bold;")),
                          
                          p("Odds ratio adalah perbandingan antara dua odds dari dua kelompok atau kondisi berbeda.
                  OR sering digunakan untuk mengukur kekuatan hubungan antara variabel independen dengan variabel dependen.",
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          ),
                          
                          tags$div(
                            style = "font-size: 20px;",
                            withMathJax("$$\\text{Odds Ratio (OR)} = \\frac{\\text{Odds Kelompok 1}}{\\text{Odds Kelompok 2}}$$")
                          ),
                          
                          
                          tags$ul(
                            tags$li("Jika OR > 1: Peluang kejadian lebih besar pada kelompok pertama."),
                            tags$li("Jika OR = 1: Tidak ada perbedaan peluang kejadian antara kelompok."),
                            tags$li("Jika OR < 1: Peluang kejadian lebih kecil pada kelompok pertama."),
                            
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                            
                          ),
                          
                          tags$br(),
                          
                          tags$h5(tags$strong("Log-Odds (Logit)"), style = "font-size: 20px; font-weight: bold;"),
                          
                          p("Log-odds atau logit adalah logaritma natural dari odds. Dalam regresi logistik,
                  log-odds digunakan untuk memodelkan hubungan linier antara variabel independen (predictors)
                  dengan variabel dependen biner.",
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"),
                          
                          tags$div(
                            style = "font-size: 20px;",
                            withMathJax("$$\\text{Log-Odds} = \\ln(\\text{Odds}) = \\ln\\left(\\frac{P}{1-P}\\right)$$")
                          ),
                          
                        )
                      ),
                      
                      
                      bsCollapsePanel(
                        title = tags$span("🔢 Evaluasi Regresi Logistik", class = "panel-title-custom"),
                        value = "panel3",
                        
                        wellPanel(
                          style = "
                background: linear-gradient(145deg, #789DBC, #FEF9F2, #C9E9D2); 
                border-radius: 15px; 
                color: black;
                border: none;",
                          
                          p("Confusion matrix memberikan informasi perbandingan hasil
                  klasifikasi model dengan data aktual.",
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"),
                          
                          tags$ul(
                            tags$li(tags$strong("Accuracy:"), "menggambarkan seberapa akurat model dapat mengklasifikasikan dengan benar.
                          Maka, accuracy merupakan rasio prediksi benar (positif dan negatif) dengan keseluruhan data.
                          Dengan kata lain, accuracy merupakan tingkat kedekatan nilai prediksi dengan nilai aktual (sebenarnya)."),
                            
                            withMathJax("$$\\text{Accuracy} = \\frac{TP + TN}{TP + TN + FP + FN}$$"),
                            tags$li(tags$strong("Precision:"), " menggambarkan tingkat keakuratan antara data yang diminta
                          dengan hasil prediksi yang diberikan oleh model. Maka, precision merupakan rasio prediksi benar
                          positif dibandingkan dengan keseluruhan hasil yang diprediksi positf. Dari semua kelas positif yang
                          telah di prediksi dengan benar, berapa banyak data yang benar-benar positif."),
                            
                            
                            withMathJax("$$\\text{Precision} = \\frac{TP}{TP + FP}$$"),
                            tags$li(tags$strong("Recall/Sensitivity:"), " menggambarkan keberhasilan model dalam menemukan kembali sebuah
                          informasi. Maka, recall merupakan rasio prediksi benar positif dibandingkan dengan keseluruhan data yang
                          benar positif."),
                            
                            withMathJax("$$\\text{Recall} = \\frac{TP}{TP + FN}$$"),
                            tags$li(tags$strong("Specificity:"), " merupakan metrik evaluasi yang menunjukkan seberapa efektif suatu model 
                          dalam mengklasifikasikan kelas negatif secara akurat."),
                            
                            withMathJax("$$\\text{Specificity} = \\frac{TN}{TN + FP}$$"),
                            tags$li(tags$strong("Balanced Accuracy:"), "adalah rata-rata semua contoh positif yang terdeteksi dengan benar 
                          dan semua contoh negatif yang terdeteksi dengan benar."),
                            
                            withMathJax("$$\\text{Balanced Accuracy} = \\frac{\\text{Sensitivity} + \\text{Specificity}}{2}$$"),
                            
                            tags$li(tags$strong("Cohen's Kappa:"), " adalah metrik yang digunakan untuk mengukur tingkat persetujuan antara 
                          dua penilai yang dapat menjadi alat yang berguna untuk mengukur kinerja model klasifikasi."),
                            withMathJax("$$\\kappa = \\frac{P_o - P_e}{1 - P_e}$$"),
                            
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          ),
                          
                          tags$br(),
                          
                          tags$h5(tags$strong("AIC dan BIC"), style = "font-size: 20px; font-weight: bold;"),
                          tags$ul(
                            tags$li(tags$strong("AIC:"), " Metrik untuk mengevaluasi keseimbangan antara performa model dan
                          kompleksitas model. AIC dihitung dengan rumus:", 
                                    tags$code("AIC = -2 * log-likelihood + 2 * k"), 
                                    " di mana ", tags$em("k"), " adalah jumlah parameter model."),
                            
                            tags$li(tags$strong("BIC:"), " Metrik Bayesian untuk mengestimasi kemungkinan maksimum dengan penalti 
                          kompleksitas model. BIC dihitung dengan rumus:",
                                    
                                    tags$code("BIC = -2 * log-likelihood + k * log(n)"), 
                                    " di mana ", tags$em("n"), " adalah jumlah pengamatan."),
                            
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                            
                          ),
                          
                          tags$br(),
                          
                          tags$h5(tags$strong("Residual Deviance"), style = "font-size: 20px; font-weight: bold;"),
                          p("Residual deviance adalah ukuran dalam regresi logistik yang menunjukkan seberapa
                  baik model sesuai dengan data aktual. Nilai ini mencerminkan ketidaksesuaian antara model dan data.",
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;")
                          
                        )
                      ),
                      
                      
                      bsCollapsePanel(
                        title = tags$span("📚 Istilah Statistika", class = "panel-title-custom"),
                        value = "panel4",
                        
                        wellPanel(
                          style = "
                background: linear-gradient(145deg, #789DBC, #FEF9F2, #C9E9D2); 
                border-radius: 15px; 
                color: black;
                border: none;",
                          
                          p("Statistik deskriptif adalah cabang statistik yang digunakan untuk mengumpulkan, meringkas, dan 
                  menyajikan data dalam bentuk yang mudah dipahami tanpa membuat generalisasi atau kesimpulan.
                  Tujuannya adalah memberikan gambaran atau ringkasan dari data yang ada.",
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"),
                          
                          tags$h5(tags$strong("Istilah dalam Statistika Deskriptif:"), style = "font-size: 20px; font-weight: bold;"),
                          
                          tags$ul(
                            tags$li(tags$strong("Mean (Rata-rata)"), ": Mean adalah nilai rata-rata yang dihitung dengan menjumlahkan semua
                          data kemudian membaginya dengan jumlah observasi."),
                            tags$li(tags$strong("Median"), ": Median adalah nilai tengah dalam data yang telah diurutkan. 
                          Jika jumlah data ganjil, median adalah nilai di posisi tengah. Jika jumlah data genap,
                          median adalah rata-rata dari dua nilai tengah."),
                            tags$li(tags$strong("Modus"), ": Modus adalah nilai yang paling sering muncul dalam data. Data bisa memiliki 
                          satu modus (unimodal), dua modus (bimodal), atau lebih (multimodal). Jika tidak ada nilai yang berulang, 
                          data tidak memiliki modus."),
                            tags$li(tags$strong("Kuartil Pertama (Q1)"), ": Q1 adalah nilai yang membagi 25% data terkecil dari 75% data 
                          lainnya dalam kumpulan data yang sudah diurutkan. Dengan kata lain, Q1 adalah median
                          dari separuh bawah data."),
                            tags$li(tags$strong("Kuartil Ketiga (Q3)"), ": Q3 adalah nilai yang membagi 75% data terkecil dari 25% data 
                          lainnya dalam kumpulan data yang sudah diurutkan. Q3 adalah median dari separuh atas data."),
                            tags$li(tags$strong("Nilai Minimum"), ": Nilai minimum adalah nilai terkecil dalam suatu kumpulan data. Ini 
                          menunjukkan batas bawah data."),
                            tags$li(tags$strong("Nilai Maksimum"), ": Nilai maksimum adalah nilai terbesar dalam suatu kumpulan data. 
                          Ini menunjukkan batas atas data."),
                            tags$li(tags$strong("Varians"), ": Varians adalah ukuran rata-rata kuadrat dari selisih setiap data 
                          terhadap mean. Varians menunjukkan seberapa jauh data tersebar dari rata-ratanya."),
                            tags$li(tags$strong("Standar Deviasi"), ": Standar deviasi adalah akar kuadrat dari varians. Standar deviasi 
                          menunjukkan seberapa jauh rata-rata data dari nilai mean dalam satuan yang sama dengan data aslinya."),
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                            
                          ),
                          
                          
                          p("Variabel dalam statistik:", 
                            style = "font-size: 20px; font-weight: bold;"),
                          
                          tags$ul(
                            tags$li(tags$strong("Variabel Kategorik"), ": Variabel kategorik merupakan variabel kualitatif hasil 
                          dari pengklasifikasian (penggolongan) suatu data. Variabel kategorik umumnya berisi 
                          variabel yang berskala nominal dan ordinal Misalnya: jenis kelamin, pendidikan, pekerjaan, dll."),
                            tags$li(tags$strong("Variabel Numerik"), ": Variabel numerik merupakan variabel kuantitatif hasil dari 
                          perhitungan dan pengukuran. variabel numerik terdiri dari variabel yang berskala interval dan rasio."),
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          ),
                          
                          tags$h5(tags$strong("Korelasi:"), style = "font-size: 20px; font-weight: bold;"),
                          
                          p("Korelasi berfungsi untuk menguji ada atau tidaknya hubungan serta arah hubungan
                  dari dua variabel atau lebih. Besar kecilnya hubungan antara dua variabel dinyatakan dalam
                  bilangan yang disebut Koefisien Korelasi yang bernilai antara -1 0 +1.", 
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"),
                          
                          tags$ul(
                            tags$li(tags$strong("Korelasi Pearson (Pearson Correlation)"), ": adalah jenis korelasi yang digunakan 
                          untuk mengukur hubungan di antara hasil-hasil suatu pengamatan dari populasi dengan 2 varian (bivariate), 
                          berdistribusi normal. Biasanya juga digunakan untuk rasio atau data berskala interval."),
                            tags$li(tags$strong("Korelasi Spearman (Spearman Correlation)"), ":adalah korelasi yang dipakai 
                          untuk mengukur keeratan hubungan antara hasil pengamatan dari populasi yang memiliki 2 varian yang 
                          berdistribusi tidak normal. Biasanya jenis korelasi ini digunakan untuk data berskala ordinal."),
                            style = "font-size: 16px; color: black; line-height: 1.3; font-family: 'Frutiger', sans-serif;"
                          )
                          
                          
                          
                        )
                      )
                      
                      
                      
                      
                    )
                  )
                )
                
                
                
              )
            )
          )
        )
      ),
      
      
      ################### TAB ITEM : OVERVIEW ################      
      tabItem(tabName = "overview",
              
              fluidRow(
                column(3, div(class = "modern-card", htmlOutput("total_customers"))),
                column(3, div(class = "modern-card", htmlOutput("mean_travel_freq"))),
                column(3, div(class = "modern-card", htmlOutput("mean_vacation_budget"))),
                column(3, div(class = "modern-card", htmlOutput("mean_income")))
              ),
              
              br(),  # Menambahkan spasi vertikal antar fluidRow
              
              fluidRow(
                box(title = tags$span(
                  tags$img(src = "versus.png", height = "40px", width = "40px"),
                  tags$span(
                    "Preferensi Liburan: Pantai vs Gunung", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotlyOutput("preference_pie", height='250px'), width = 4),
                
                box(title = tags$span(
                  tags$img(src = "weather.png", height = "40px", width = "40px"),
                  tags$span(
                    "Musim Favorit", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotlyOutput("favorite_season", height = "250px"), width = 4),
                
                
                box(title = tags$span(
                  tags$img(src = "calendar.png", height = "40px"),
                  tags$span(
                    "Preferensi Aktivitas", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotlyOutput("activity", height = "250px"), width = 4)
                
              ),
              
              
              fluidRow(
                box(title = tags$span(
                  tags$img(src = "education.png", height = "40px", width = "40px"),
                  tags$span(
                    "Tingkat Pendidikan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotlyOutput("education_preference", height = "250px",), width = 4),
                
                box(title = tags$span(
                  tags$img(src = "gender-equality.png", height = "40px", width = "40px"),
                  tags$span(
                    "Jenis Kelamin", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotlyOutput("gender_preference", height = "250px"), width = 4),
                
                
                box(title = tags$span(
                  tags$img(src = "fiat-money.png", height = "40px", width = "40px"),
                  tags$span(
                    "Anggaran Liburan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), status = "primary", solidHeader = TRUE, 
                plotlyOutput("budget", height = "250px"), width = 4)
              )
      ),
      
      
      ################### TAB ITEM : EDA ################
      tabItem(tabName = "eda",
              fluidRow(
                # Box for file upload
                box(
                  title = tags$span(
                    "Pilih Dataset", 
                    style = "
              color: white; 
              font-weight: bold; 
              font-size: 20px; 
              text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                           -2px -2px 4px rgba(0, 0, 0, 0.5), 
                           2px -2px 4px rgba(0, 0, 0, 0.5), 
                           -2px 2px 4px rgba(0, 0, 0, 0.5);
              "
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  radioButtons("data_source2", "Sumber Data",
                               choices = c("Data saat ini (Dashboard)" = "example",
                                           "Unggah Data" = "upload2")),
                  conditionalPanel(
                    condition = "input.data_source2 == 'upload2'",
                    fileInput("file2", "Unggah File CSV", accept = ".csv")
                  ),
                  actionButton("load_data2", "Muat Data"),
                  checkboxInput('close_data_table2', 'Tutup Tampilan Data', value=F)
                ),
                
                box(title = tags$span(
                  tags$span(
                    "Dataset", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 8,dataTableOutput("data_table2")
                )
                
                
                
              ),
              
              # Sidebar Layout for inputs and outputs (Statistik Deskriptif and Grafik)
              fluidRow(
                # Box for Analisis Deskriptif
                box(
                  title = tags$span(
                    "Pilih Jenis Variabel", 
                    style = "
              color: white; 
              font-weight: bold; 
              font-size: 20px; 
              text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                           -2px -2px 4px rgba(0, 0, 0, 0.5), 
                           2px -2px 4px rgba(0, 0, 0, 0.5), 
                           -2px 2px 4px rgba(0, 0, 0, 0.5);
              "
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 2, height='400px',
                  
                  # Pilih jenis statistik dan variabel
                  selectInput("summary_type", "Pilih Jenis Statistik:",
                              choices = c("Numerik" = "numeric", "Kategorik" = "categorical")),
                  
                  uiOutput("summary_var_ui"),  # Dinamis untuk variabel yang muncul sesuai skala yang dipilih
                  selectInput("color_palette", "Pilih Skema Warna:",
                              choices = rownames(brewer.pal.info), selected = "Set1")
                ),
                
                # Box untuk Hasil Statistik Deskriptif
                box(
                  title = tags$span(
                    "Statistik Deskriptif", 
                    style = "
        color: white; 
        font-weight: bold; 
        font-size: 20px; 
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                     -2px -2px 4px rgba(0, 0, 0, 0.5), 
                     2px -2px 4px rgba(0, 0, 0, 0.5), 
                     -2px 2px 4px rgba(0, 0, 0, 0.5);
      "
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3, height='480px',
                  htmlOutput("summary_output")  # Hasil statistik
                ),
                
                
                box(
                  title = tags$span(
                    "Grafik (Univariat)", 
                    style = "
              color: white; 
              font-weight: bold; 
              font-size: 20px; 
              text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                           -2px -2px 4px rgba(0, 0, 0, 0.5), 
                           2px -2px 4px rgba(0, 0, 0, 0.5), 
                           -2px 2px 4px rgba(0, 0, 0, 0.5);
              "
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 7, height='480px',
                  plotlyOutput("plot_deskriptif")  # Grafik Plotly
                )
              ),
              
              fluidRow(
                # Box for selecting X and Y variables
                box(
                  title = tags$span(
                    "Pilih Variabel", 
                    style = "
                      color: white; 
                      font-weight: bold; 
                      font-size: 20px; 
                      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5),
                      -2px -2px 4px rgba(0, 0, 0, 0.5),
                      2px -2px 4px rgba(0, 0, 0, 0.5),
                      -2px 2px 4px rgba(0, 0, 0, 0.5);
                    "
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 2, height='400px',
                  selectInput("x_var", "Pilih Variabel X:", choices = NULL),
                  selectInput("y_var", "Pilih Variabel Y:", choices = NULL),
                  selectInput("color_palette2", "Pilih Skema Warna:",
                              choices = rownames(brewer.pal.info), selected = "Set1")
                ),
                
                box(
                  title = tags$span(
                    "Grafik (Bivariat)", 
                    style = "
                      color: white; 
                      font-weight: bold; 
                      font-size: 20px; 
                      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5),
                      -2px -2px 4px rgba(0, 0, 0, 0.5),
                      2px -2px 4px rgba(0, 0, 0, 0.5),
                      -2px 2px 4px rgba(0, 0, 0, 0.5);
                    "
                  ),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6, height='450px',
                  plotlyOutput("plot_output", height = '370px')  # Grafik Plotly
                ),
                
                # Box for Analysis Result (ANOVA / Chi-Square)
                box(
                  title = tags$span(
                    "Hasil ANOVA / Uji Chi-square", 
                    style = "
                      color: white; 
                      font-weight: bold; 
                      font-size: 20px; 
                      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5),
                      -2px -2px 4px rgba(0, 0, 0, 0.5),
                      2px -2px 4px rgba(0, 0, 0, 0.5),
                      -2px 2px 4px rgba(0, 0, 0, 0.5);
                    "),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4, height='450px',
                  htmlOutput("analysis_result")  # Tampilkan hasil analisis
                )
              ),
              
              fluidRow(
                # Box untuk memilih metode korelasi
                box(
                  title = tags$span(
                    "Pilih Korelasi", 
                    style = "
                      color: white; 
                      font-weight: bold; 
                      font-size: 20px; 
                      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5),
                      -2px -2px 4px rgba(0, 0, 0, 0.5),
                      2px -2px 4px rgba(0, 0, 0, 0.5),
                      -2px 2px 4px rgba(0, 0, 0, 0.5);
                    "),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 2, height='400px',
                  radioButtons("cor_method", "Pilih Metode Korelasi:",
                               choices = c("Pearson" = "pearson", "Spearman" = "spearman"),
                               selected = "pearson"),
                  selectInput("color_palette3", "Pilih Skema Warna:",
                              choices = rownames(brewer.pal.info), selected = "RdBu")
                ),
                
                # Box untuk heatmap
                box(
                  title = tags$span(
                    "Heatmap Korelasi", 
                    style = "
                      color: white; 
                      font-weight: bold; 
                      font-size: 20px; 
                      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5),
                      -2px -2px 4px rgba(0, 0, 0, 0.5),
                      2px -2px 4px rgba(0, 0, 0, 0.5),
                      -2px 2px 4px rgba(0, 0, 0, 0.5);
                    "),
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6, height='500px',
                  plotlyOutput("correlation_heatmap", width = "90%", height = "400px")
                )
              )
      ),
      
      
      #========================REGRESI LOGISTIK======================================#      
      # Tab Regresi Logistik
      tabItem(tabName = "logistic_regression",
              fluidRow(
                box(title = tags$span(
                  tags$span(
                    "Pilih Dataset", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 4,
                radioButtons("data_source", "Sumber Data",
                             choices = c("Data saat ini (Dashboard)" = "example",
                                         "Unggah Data" = "upload")),
                conditionalPanel(
                  condition = "input.data_source == 'upload'",
                  fileInput("file", "Unggah File CSV", accept = ".csv")
                ),
                actionButton("load_data", "Muat Data"),
                checkboxInput('close_data_table', 'Tutup Tampilan Data', value=F)
                ),
                
                box(title = tags$span(
                  tags$span(
                    "Dataset", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 8,dataTableOutput("data_table")
                )
                
                
              ),
              
              fluidRow(
                box(title = tags$span(
                  tags$span(
                    "Pengaturan", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 4,
                uiOutput("select_response"),
                uiOutput("select_predictors"),
                # Tombol Select All dan Unselect All
                actionButton(inputId = "add_all", 
                             label = HTML('<i class="fas fa-check"></i> Pilih Semua'),
                             class = "custom-button"),
                actionButton(inputId = "remove_all", 
                             label = HTML('<i class="fas fa-times"></i> Batal Pilih Semua'),
                             class = "custom-button"),
                
                radioButtons("coef_format", "Format Koefisien", 
                             choices = c("Asli (Log-odds)" = "original", "Odds-Ratio" = "odds_ratio"),
                             inline = TRUE),
                selectInput("confidence_level", "Pilih Tingkat Kepercayaan",
                            choices = c("0.90", "0.95", "0.99"), selected = "0.95"),
                
                # Evaluasi Model
                tags$p("Multikolinearitas dan Evaluasi Model",
                       style = "
                  color: black; 
                  font-size: 14px; 
                  font-weight: bold; 
                  text-align: left;
                  margin-top: 30px;
                  margin-bottom: 0px;"),
                
                checkboxInput('multikol', 'Cek Multikolinearitas', value=F),
                checkboxInput('eval_model', 'AIC-BIC/Deviance', value=F),
                checkboxInput('conf_mat', 'Confusion Matrix', value=F),
                
                
                # Menambahkan margin vertikal antar tombol
                tags$style(HTML(
                  "
                  #selected_predictors 
                  {
                    margin-bottom: 1px;  /* Jarak kecil antara selected_predictors dan tombol */
                  }
                  
                  .custom-button {
                    margin-bottom: 15px;  /* Jarak kecil antara tombol add_all dan remove_all */
                  }
                  
                  #coef_format {
                    margin-bottom: 15px;  /* Jarak besar antara add_all/remove_all dan coef_format */
                  }
                  
                  #confidence_level
                  {
                    margin-top: 1px;  /* Jarak besar antara coef_format dan confidence_level */
                  }
                  
                  #multikol
                  {
                    margin-top: 1px;  /* Jarak besar antara multikol dan confidence_level */
                  }

                  
                "
                )),
                
                actionButton("fit_model", "Jalankan Model")
                
                ),
                
                box(title = tags$span(
                  tags$span(
                    "Hasil Regresi Logistik", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 8,
                DT::dataTableOutput("logistic_summary"),
                downloadButton("download_csv", "Unduh CSV"))
              ),
              
              
              fluidRow(
                
                box(title = tags$span(
                  tags$span(
                    "Pemeriksaan Multikolinearitas", 
                    style = "
    color: white; 
    font-weight: bold; 
    font-size: 20px; 
    text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5), 
                 -2px -2px 4px rgba(0, 0, 0, 0.5), 
                 2px -2px 4px rgba(0, 0, 0, 0.5), 
                 -2px 2px 4px rgba(0, 0, 0, 0.5);
  "
                  )
                ), width = 4,
                DTOutput("vif_table")),
                
                box(
                  title = tags$span(
                    tags$span(
                      "Nilai AIC BIC Deviance", 
                      style = "color: white; font-weight: bold; font-size: 20px; text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5)"
                    )
                  ),
                  width = 4,
                  uiOutput("aic_deviance")
                ),
                
                
                box(
                  title = tags$span(
                    tags$span(
                      "Confusion Matrix", 
                      style = "color: white; font-weight: bold; font-size: 20px; text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5)"
                    )
                  ),
                  width = 4,
                  plotOutput("conf_matrix", height = "300px"),
                  uiOutput("conf_matrix_acc")
                )
                
                
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filter data based on user inputs
  filtered_data <- reactive({
    data <- vacation_data
    
    if (input$season_filter != "Semua") {
      data <- data[data$Musim_Favorit == input$season_filter, ]
    }
    
    data <- data[data$Umur >= input$age_filter[1] & data$Umur <= input$age_filter[2], ]
    
    if (input$location_filter != "Semua") {
      data <- data[data$Lokasi == input$location_filter, ]
    }
    
    return(data)
  })
  
  ############################# OUTPUT: OVERVIEW #############################
  # VISUALISASI CARD BARIS PERTAMA 
  output$total_customers <- renderUI({
    data <- filtered_data()
    HTML(paste0(
      "<i class='fas fa-users'></i>", # Ikon Font Awesome
      "<h3>Total Pelanggan</h3>",
      "<span>", format(nrow(data), big.mark = ",", decimal.mark = "."), " Orang</span>"
    ))
  })
  
  output$mean_travel_freq <- renderUI({
    data <- filtered_data()
    travel_freq <- round(mean(data$Frekuensi_Liburan, na.rm = TRUE),1)
    
    HTML(paste0(
      "<i class='fas fa-plane-departure'></i>", # Ikon Font Awesome untuk perjalanan
      "<h3>Rata-rata Frekuensi Liburan per Orang</h3>",
      "<span>", 
      format(travel_freq, big.mark = ",", decimal.mark = "."), "x per Tahun </span>"
    ))
  })
  
  
  output$mean_vacation_budget <- renderUI({
    data <- filtered_data()
    vacation_budget <- round(mean(data$Anggaran_Liburan, na.rm = TRUE), 1)
    
    HTML(paste0(
      "<i class='fas fa-wallet'></i>", # Ikon Font Awesome untuk dompet
      "<h3>Rata-rata Anggaran Dana per Liburan</h3>",
      "<span>", 
      format(vacation_budget, big.mark = ",", decimal.mark = "."), " USD</span>"
    ))
  })
  
  
  output$mean_income <- renderUI({
    data <- filtered_data()
    income <- round(mean(data$Penghasilan, na.rm = TRUE), 1)
    
    HTML(paste0(
      "<i class='fas fa-money-bill-wave'></i>", # Ikon Font Awesome untuk uang
      "<h3>Rata-rata Penghasilan Tahunan per Orang</h3>",
      "<span>", 
      format(income, big.mark = ",", decimal.mark = "."), " USD</span>"
    ))
  })
  
  # Plot 1: Pie Chart (Total dan Persentase Pantai vs Gunung)
  output$preference_pie <- renderPlotly({
    data <- filtered_data()
    
    df_pie <- data.frame(table(data$Preferensi_Liburan))
    colnames(df_pie) <- c('Preferensi Liburan', 'Total')
    
    plot_ly(df_pie, labels = ~`Preferensi Liburan`, values = ~Total, type = 'pie', 
            marker = list(colors = brewer.pal(5, input$color_scheme)[1:2]), 
            hole = 0.4) %>%
      layout(
        title = list(
          font = list(size = 20, family = "Arial", weight = "bold")
        ),  # Mengatur ukuran font untuk judul, font family dan bold
        font = list(size = 20, family = "Arial", weight = "bold"),  # Mengatur font label menjadi bold
        showlegend = TRUE,  # Menampilkan legend
        legend = list(font = list(size = 17, family = "Arial", weight = "bold")),  # Mengatur font untuk legend
        hoverlabel = list(
          font = list(size = 17, family = "Arial", weight = "bold")  # Font bold untuk hover label
        )
      )
  })
  
  
  # Plot 2: Stacked Bar Plot - Pendidikan (Menggunakan Plotly dengan RColorBrewer)
  output$education_preference <- renderPlotly({
    data <- filtered_data()
    education_data <- data %>% 
      group_by(Pendidikan, Preferensi_Liburan) %>%
      tally(name = "n") %>%
      group_by(Preferensi_Liburan) %>%
      mutate(pct = n / sum(n) * 100,  # Hitung persentase
             label_text = paste0(round(pct, 1), "%"))  # Format teks persentase
    
    # Mendapatkan palet warna dari RColorBrewer
    colors <- brewer.pal(5, input$color_scheme)
    
    # Menghitung posisi tengah setiap segmen bar
    education_data <- education_data %>%
      group_by(Preferensi_Liburan) %>%
      mutate(y_mid = cumsum(n) - (n / 2))  # Titik tengah
    
    # Membuat stacked bar plot
    plot <- plot_ly(
      data = education_data,
      x = ~Preferensi_Liburan,
      y = ~n,
      color = ~Pendidikan,
      colors = colors,
      type = "bar",
      hoverinfo = "x+y+text"  # Hover menampilkan info x, y, dan teks
    )
    
    # Menambahkan label menggunakan annotations
    annotations <- list()
    for (i in 1:nrow(education_data)) {
      annotations[[i]] <- list(
        x = education_data$Preferensi_Liburan[i],
        y = education_data$y_mid[i],  # Gunakan titik tengah
        text = education_data$label_text[i],
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      )
    }
    
    # Menambahkan layout dan label
    plot %>%
      layout(
        xaxis = list(title = "", 
                     titlefont = list(size = 18, family = "Arial", weight = "bold"),
                     tickfont = list(size = 16, family = "Arial", weight = "bold")),
        yaxis = list(title = "Jumlah Orang", 
                     titlefont = list(size = 18, family = "Arial", weight = "bold"),
                     tickfont = list(size = 16, family = "Arial", weight = "bold")),
        barmode = 'stack', 
        legend = list(title = list(text = 'Pendidikan', font = list(size = 16, family = "Arial", weight = "bold")),
                      font = list(size = 16, family = "Arial", weight = "bold")),
        margin = list(l = 40, r = 40, t = 40, b = 40),
        annotations = annotations  # Tambahkan anotasi
      )
  })
  
  
  # Plot 3: Stacked Bar Plot - Jenis Kelamin
  output$gender_preference <- renderPlotly({
    data <- filtered_data()
    
    # Gender per preference (Beach and Mountain)
    gender_data <- data %>% 
      group_by(Preferensi_Liburan, Jenis_Kelamin) %>%
      tally(name = "n") %>%
      group_by(Preferensi_Liburan) %>%
      mutate(pct = n / sum(n) * 100,  # Hitung persentase
             label_text = paste0(round(pct, 1), "%"))  # Format teks persentase
    
    # Mendapatkan palet warna dari RColorBrewer
    colors <- brewer.pal(5, input$color_scheme)
    
    # Menghitung posisi tengah setiap segmen bar
    gender_data <- gender_data %>%
      group_by(Preferensi_Liburan) %>%
      mutate(y_mid = cumsum(n) - (n / 2))  # Titik tengah
    
    # Membuat stacked bar plot
    plot <- plot_ly(
      data = gender_data,
      x = ~Preferensi_Liburan,
      y = ~n,
      color = ~Jenis_Kelamin,
      colors = colors,
      type = "bar",
      hoverinfo = "x+y+text"  # Hover menampilkan info x, y, dan teks
    )
    
    # Menambahkan label menggunakan annotations
    annotations <- list()
    for (i in 1:nrow(gender_data)) {
      annotations[[i]] <- list(
        x = gender_data$Preferensi_Liburan[i],
        y = gender_data$y_mid[i],  # Gunakan titik tengah
        text = gender_data$label_text[i],
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      )
    }
    
    # Menambahkan layout dan label
    plot %>%
      layout(
        xaxis = list(title = "", 
                     titlefont = list(size = 18, family = "Arial", weight = "bold"),
                     tickfont = list(size = 16, family = "Arial", weight = "bold")),
        yaxis = list(title = "Jumlah Orang", 
                     titlefont = list(size = 18, family = "Arial", weight = "bold"),
                     tickfont = list(size = 16, family = "Arial", weight = "bold")),
        barmode = 'stack', 
        legend = list(title = list(text = 'Jenis Kelamin', font = list(size = 16, family = "Arial", weight = "bold")),
                      font = list(size = 16, family = "Arial", weight = "bold")),
        margin = list(l = 40, r = 40, t = 40, b = 40),
        annotations = annotations  # Tambahkan anotasi
      )
  })
  
  
  # Plot 4 - Musim Favorit
  output$favorite_season <- renderPlotly({
    data <- filtered_data()
    season_data <- data %>% 
      group_by(Musim_Favorit, Preferensi_Liburan) %>%
      tally(name = "n") %>%
      group_by(Musim_Favorit) %>%
      mutate(pct = n / sum(n) * 100,  # Hitung persentase
             label_text = paste0(round(pct, 1), "%"))  # Format teks persentase
    
    # Mendapatkan palet warna dari RColorBrewer
    colors <- brewer.pal(5, input$color_scheme)[1:2]
    
    # Menghitung posisi tengah setiap segmen bar (untuk label)
    season_data <- season_data %>%
      group_by(Musim_Favorit) %>%
      mutate(x_mid = cumsum(n) - (n / 2))  # Titik tengah pada sumbu X
    
    # Membuat stacked bar plot horizontal
    plot <- plot_ly(
      data = season_data,
      y = ~Musim_Favorit,  # Menggunakan Musim_Favorit pada sumbu Y
      x = ~n,  # Menggunakan n pada sumbu X
      color = ~Preferensi_Liburan,
      colors = colors,
      type = "bar",
      orientation = "h",  # Mengatur orientasi bar menjadi horizontal
      hoverinfo = "y+x+text"  # Hover menampilkan info y, x, dan teks
    )
    
    # Menambahkan label menggunakan annotations
    annotations <- list()
    for (i in 1:nrow(season_data)) {
      annotations[[i]] <- list(
        x = season_data$x_mid[i],  # Gunakan titik tengah pada sumbu X
        y = season_data$Musim_Favorit[i],
        text = season_data$label_text[i],
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      )
    }
    
    # Menambahkan layout dan label dengan margin pada sumbu Y untuk memberi ruang
    plot %>%
      layout(
        xaxis = list(title = "Jumlah Orang", 
                     titlefont = list(size = 18, family = "Arial", weight = "bold"),
                     tickfont = list(size = 16, family = "Arial", weight = "bold")),
        yaxis = list(
          title = "", 
          titlefont = list(size = 18, family = "Arial", weight = "bold"),
          tickfont = list(size = 16, family = "Arial", weight = "bold"),
          tickmode = "array",  # Mengatur mode tick secara manual
          tickvals = season_data$Musim_Favorit,  # Menentukan nilai tick
          ticktext = season_data$Musim_Favorit,  # Menampilkan label tanpa tambahan spasi
          ticklen = 3,  # Memberi panjang garis pada tick
          tickpad = 10  # Memberikan jarak antar label dan sumbu Y
        ),
        barmode = 'stack', 
        legend = list(title = list(text = 'Preferensi Liburan', font = list(size = 16, family = "Arial", weight = "bold")),
                      font = list(size = 16, family = "Arial", weight = "bold")),
        margin = list(l = 60, r = 40, t = 40, b = 40),  # Menambahkan margin kiri lebih besar
        annotations = annotations  # Tambahkan anotasi
      )
  })
  
  
  # # Plot 5:  Preferensi Aktivitas
  output$activity <-  renderPlotly({
    data <- filtered_data()  # Data yang sudah difilter
    
    # Menghitung jumlah dan persentase untuk setiap kategori preferensi_aktivitas
    activity_data <- data %>%
      count(Preferensi_Aktivitas)
    
    # Mendapatkan palet warna dari RColorBrewer
    colors <- brewer.pal(5, input$color_scheme)
    
    # Membuat bar plot
    plot <- plot_ly(
      data = activity_data,
      x = ~Preferensi_Aktivitas,  # Preferensi aktivitas di sumbu X
      y = ~n,  # Jumlah orang di sumbu Y
      type = "bar",
      marker = list(color = colors),  # Warna bar
      hoverinfo = "x+y+text",  # Hover menampilkan info
      text = ~n,  # Teks yang akan ditampilkan di hover
      textposition = "inside"  # Menampilkan teks di luar bar
    )
    
    # Menambahkan layout dan label
    plot %>%
      layout(
        xaxis = list(
          title = "Preferensi Aktivitas",
          titlefont = list(size = 18, family = "Arial", weight = "bold"),
          tickfont = list(size = 16, family = "Arial", weight = "bold")
        ),
        yaxis = list(
          title = "Jumlah Orang",
          titlefont = list(size = 18, family = "Arial", weight = "bold"),
          tickfont = list(size = 16, family = "Arial", weight = "bold")
        ),
        barmode = "group",  # Set mode menjadi group jika kategori lebih dari satu
        margin = list(l = 60, r = 40, t = 40, b = 40),  # Margin agar plot tidak terpotong
        showlegend = FALSE  # Tidak menampilkan legend
      )
  })
  
  # Plot 6 Anggaran Liburan
  output$budget <- renderPlotly({
    data <- filtered_data()
    data$KategoriAnggaran <- cut(data$Anggaran_Liburan, 
                                 breaks = c(-Inf, 1500, 3000, 4000, Inf), 
                                 labels = c("<1500", "1500-3000", "3000-4000", ">4000"),
                                 right = FALSE)
    
    budget_data <- data %>% 
      group_by(KategoriAnggaran, Preferensi_Liburan) %>%
      tally(name = "n") %>%
      group_by(Preferensi_Liburan) %>%
      mutate(pct = n / sum(n) * 100,  # Hitung persentase
             label_text = paste0(round(pct, 1), "%"))  # Format teks persentase
    
    # Mendapatkan palet warna dari RColorBrewer
    colors <- brewer.pal(5, input$color_scheme)
    
    # Menghitung posisi tengah setiap segmen bar
    budget_data <- budget_data %>%
      group_by(Preferensi_Liburan) %>%
      mutate(y_mid = cumsum(n) - (n / 2))  # Titik tengah
    
    # Membuat stacked bar plot
    plot <- plot_ly(
      data = budget_data,
      x = ~Preferensi_Liburan,
      y = ~n,
      color = ~KategoriAnggaran,
      colors = colors,
      type = "bar",
      hoverinfo = "x+y+text"  # Hover menampilkan info x, y, dan teks
    )
    
    # Menambahkan label menggunakan annotations
    annotations <- list()
    for (i in 1:nrow(budget_data)) {
      annotations[[i]] <- list(
        x = budget_data$Preferensi_Liburan[i],
        y = budget_data$y_mid[i],  # Gunakan titik tengah
        text = budget_data$label_text[i],
        showarrow = FALSE,
        font = list(size = 12, color = "black")
      )
    }
    
    # Menambahkan layout dan label
    plot %>%
      layout(
        xaxis = list(title = "", 
                     titlefont = list(size = 18, family = "Arial", weight = "bold"),
                     tickfont = list(size = 14, family = "Arial", weight = "bold")),
        yaxis = list(title = "Jumlah Orang", 
                     titlefont = list(size = 18, family = "Arial", weight = "bold"),
                     tickfont = list(size = 14, family = "Arial", weight = "bold")),
        barmode = 'stack', 
        legend = list(title = list(text = 'Anggaran Liburan', font = list(size = 16, family = "Arial", weight = "bold")),
                      font = list(size = 14, family = "Arial", weight = "bold")),
        margin = list(l = 40, r = 40, t = 40, b = 40),
        annotations = annotations  # Tambahkan anotasi
      )
  })
  
  
  
  ############################# OUTPUT: EDA  #############################
  # Reactive value untuk menyimpan dataset
  data2 <- reactiveVal()
  
  # Dataset contoh
  example_data <- reactive({
    vacation_data
  })
  
  
  # Muat dataset berdasarkan pilihan
  observeEvent(input$load_data2, {
    if (input$data_source2 == "example") {
      data2(example_data())
    } else if (!is.null(input$file2)) {
      # Pengecekan ukuran file terlebih dahulu
      if (input$file2$size > 10 * 1024^2) { # 10 MB
        showModal(modalDialog(
          title = "Error",
          "Ukuran file melebihi batas maksimum 10 MB.",
          easyClose = TRUE
        ))
      } else {
        req(input$file2) # Validasi file hanya jika ukuran sesuai
        uploaded_data <- read.csv(input$file2$datapath)
        data2(uploaded_data)
      }
    }
  })
  
  
  # Tampilkan Data
  output$data_table2 <- renderDataTable({
    req(data2())
    
    if(input$close_data_table2){
      NULL
    }
    
    else{
      datatable(data2(), options = list(scrollX = TRUE,
                                        scrollY = '200px'  # Set height of the table to make it scrollable
      )
      )
    }
  })
  
  
  # Mengatur UI untuk memilih variabel sesuai jenis statistik yang dipilih
  output$summary_var_ui <- renderUI({
    req(input$summary_type, data2())
    
    if (input$summary_type == "numeric") {
      # Pilih variabel numerik
      selectInput("summary_var", "Pilih Variabel Numerik:", 
                  choices = names(data2())[sapply(data2(), is.numeric)])
    } else {
      # Pilih variabel kategorik
      selectInput("summary_var", "Pilih Variabel Kategorik:", 
                  choices = names(data2())[sapply(data2(), is.factor)])
    }
  })
  
  # Menampilkan statistik deskriptif
  output$summary_output <- renderUI({
    req(input$summary_var, data2())
    
    var_data <- data2()[[input$summary_var]]
    
    if (input$summary_type == "numeric") {
      # Statistik Deskriptif untuk numerik
      summary_stats <- summary(var_data)
      
      output_text <- paste0(
        "<div class='summary-box'><h4>Statistik Deskriptif: ", input$summary_var, "</h4>",
        "<p>Min: ", round(summary_stats[1], 2), "</p>",
        "<p>Max: ", round(summary_stats[6], 2), "</p>",
        "<p>Median: ", round(summary_stats[3], 2), "</p>",
        "<p>Mean: ", round(mean(var_data, na.rm = TRUE), 2), "</p>",
        "<p>Q1: ", round(quantile(var_data, 0.25), 2), "</p>",
        "<p>Q3: ", round(quantile(var_data, 0.75), 2), "</p>",
        "<p>Standar Deviasi: ", round(sd(var_data, na.rm = TRUE), 2), "</p>",
        "<p>Variance: ", round(var(var_data, na.rm = TRUE), 2), "</p></div>"
      )
    } else {
      # Statistik Deskriptif untuk kategorik
      freq_table <- table(var_data)
      prop_table <- prop.table(freq_table)
      
      output_text <- paste0(
        "<div class='summary-box'><h4>Statistik Deskriptif: ", input$summary_var, "</h4>",
        "<p><strong>Mode:</strong> ", names(freq_table)[which.max(freq_table)], "</p>",
        "<p><strong>Frekuensi:</strong></p>",
        "<ul>",
        paste0("<li><strong>", names(freq_table), ":</strong> ", freq_table, " (", 
               round(prop_table * 100, 2), "%)</li>", collapse = ""),
        "</ul>",
        "</div>"
      )
    }
    
    HTML(output_text)
  })
  
  # Menampilkan grafik sesuai pilihan (Histogram atau Pie Chart)
  output$plot_deskriptif <- renderPlotly({
    req(input$summary_var, data2())
    
    var_data <- data2()[[input$summary_var]]
    
    # Pilihan warna berdasarkan input
    color_palette <- input$color_palette
    
    if (input$summary_type == "numeric") {
      # Histogram untuk variabel numerik
      p <- ggplot(data2(), aes(x = var_data)) + 
        geom_histogram(bins=5, fill = RColorBrewer::brewer.pal(9, color_palette)[1], color = "white") +
        labs(title = paste("Histogram: ", input$summary_var), x='', y='Jumlah') +
        theme_minimal()
      ggplotly(p)
    } else {
      # Pie chart untuk variabel kategorik
      pie_data <- table(var_data)
      p <- plot_ly(labels = names(pie_data), values = pie_data, type = "pie",
                   marker = list(colors = RColorBrewer::brewer.pal(length(pie_data), color_palette))) %>%
        layout(title = paste("Pie Chart: ", input$summary_var))
      p
    }
  })
  
  
  
  # Update choices for numeric and categorical variables based on dataset
  observe({
    updateSelectInput(session, "x_var", choices = names(data2()))
    updateSelectInput(session, "y_var", choices = names(data2()))
  })
  
  # Plotly Boxplot for Numeric X vs Categorical Y
  output$plot_output <- renderPlotly({
    req(input$x_var, input$y_var, data2())
    
    # X numerik vs Y kategorik (Boxplot)
    if (is.numeric(data2()[[input$x_var]]) && is.factor(data2()[[input$y_var]])) {
      p <- plot_ly(
        data = data2(),
        x = ~get(input$y_var),
        y = ~get(input$x_var),
        type = "box",
        color = ~get(input$y_var),
        colors = input$color_palette2,
        showlegend = FALSE,
        jitter = 0.05,
        pointpos = 0,
        hoverinfo = "x+y+name",  # Menampilkan nama kategori, nilai X, dan Y pada hover
        name = paste(input$x_var, "vs", input$y_var)
      ) %>%
        layout(
          title = paste("Boxplot", input$x_var, "vs", input$y_var),
          xaxis = list(title = input$y_var),
          yaxis = list(title = input$x_var)
        )
      p
    }
    
    
    # X kategorik vs Y kategorik (Barplot)
    else if (is.factor(data2()[[input$x_var]]) && is.factor(data2()[[input$y_var]])) {
      # Mengelompokkan data dan menghitung frekuensi
      data_agg <- data2() %>%
        group_by_at(c(input$x_var, input$y_var)) %>%
        summarise(count = n(), .groups = "drop")
      
      # Membuat barplot menggunakan plot_ly
      p <- plot_ly(
        data = data_agg,
        x = ~get(input$x_var),  # Menggunakan get untuk mengakses kolom secara dinamis
        y = ~count,
        type = "bar",
        color = ~get(input$y_var),  
        colors = input$color_palette2,
        hoverinfo = "x+y",
        showlegend = FALSE,
        name = paste(input$x_var, "vs", input$y_var)
      ) %>%
        layout(
          title = paste("Barplot", input$x_var, "vs", input$y_var),
          xaxis = list(title = input$x_var),
          yaxis = list(title = "Count")
        )
      p
    }
    
  })
  
  
  
  # Output Hasil ANOVA atau Chi-Square
  output$analysis_result <- renderUI({
    req(input$x_var, input$y_var, data2())
    
    # ANOVA: X numerik vs Y kategorik
    if (is.numeric(data2()[[input$x_var]]) && is.factor(data2()[[input$y_var]])) {
      anova_result <- aov(data2()[[input$x_var]] ~ data2()[[input$y_var]])
      summary_anova <- summary(anova_result)
      p_value <- summary_anova[[1]]$`Pr(>F)`[1]
      conclusion <- ifelse(p_value <= 0.05,
                           paste("Kesimpulan: Terdapat hubungan yang signifikan antara", input$x_var, "dan", input$y_var, "(p <= 0.05)."),
                           paste("Kesimpulan: Tidak terdapat hubungan yang signifikan antara", input$x_var, "dan", input$y_var, "(p > 0.05)."))
      
      # Menampilkan Hasil ANOVA dengan CSS
      HTML(paste(
        "<div class='summary-box' style='background-color: #0A3981; color: white; padding: 15px;'>",
        "<h4 style='font-size: 18px;'>Hasil ANOVA:</h4>",
        "<p>F-statistic: ", round(summary_anova[[1]]$`F value`[1], 2), "</p>",
        "<p>p-value: ", round(p_value, 4), "</p>",
        "<p style='font-size: 16px; color: white;'>", conclusion, "</p>",
        "</div>"
      ))
      
    } else if (is.factor(data2()[[input$x_var]]) && is.factor(data2()[[input$y_var]])) {
      
      # Chi-Square Test
      chisq_test <- chisq.test(table(data2()[[input$x_var]], data2()[[input$y_var]]))
      
      p_value <- chisq_test$p.value
      chi_square_stat <- chisq_test$statistic
      
      if (p_value <= 0.05) {
        conclusion <- paste("Kesimpulan: Terdapat hubungan yang signifikan antara", input$x_var, "dan", input$y_var, "(p <= 0.05).")
      } else {
        conclusion <- paste("Kesimpulan: Tidak terdapat hubungan yang signifikan antara", input$x_var, "dan", input$y_var, "(p > 0.05).")
      }
      
      HTML(paste(
        "<div style='background-color: #0A3981; color: white; padding: 10px; border-radius: 10px; margin-bottom: 10px;'>",
        "<h4>Hasil Chi-Square Test:</h4>",
        "<p>Chi-Square Statistic: ", round(chi_square_stat, 3), "</p>",
        "<p>p-value: ", round(p_value, 3), "</p>",
        "<p style='font-weight: bold;'>", conclusion, "</p>",
        "</div>"
      ))
    }
  })
  
  # Reactive data for numeric columns
  numeric_data <- reactive({
    req(data2())
    data2() %>% select_if(is.numeric)
  })
  
  # Create the correlation matrix and plotly heatmap
  output$correlation_heatmap <- renderPlotly({
    req(numeric_data())
    
    # Compute the correlation matrix based on selected method (Pearson/Spearman)
    correlation_matrix <- cor(numeric_data(), method = input$cor_method, use = "complete.obs")
    
    # Plotly heatmap with ColorBrewer palette
    plot_ly(
      z = correlation_matrix,
      x = colnames(correlation_matrix),
      y = rownames(correlation_matrix),
      type = "heatmap",
      colors = brewer.pal(n = 8, name = input$color_palette3),
      colorbar = list(title = "Korelasi"),
      showscale = TRUE
    ) %>%
      layout(
        title = paste("Korelasi", toupper(input$cor_method), "Antar Variabel"),
        xaxis = list(title = "Variabel"),
        yaxis = list(title = "Variabel")
      )
  })
  
  
  
  
  ############################# OUTPUT: Regresi Logistik  #############################
  # Reactive value untuk menyimpan dataset
  data <- reactiveVal()
  
  
  # Muat dataset berdasarkan pilihan
  observeEvent(input$load_data, {
    if (input$data_source == "example") {
      data(example_data())
    } else if (!is.null(input$file)) {
      # Pengecekan ukuran file terlebih dahulu
      if (input$file$size > 10 * 1024^2) { # 10 MB
        showModal(modalDialog(
          title = "Error",
          "Ukuran file melebihi batas maksimum 10 MB.",
          easyClose = TRUE
        ))
      } else {
        req(input$file) # Validasi file hanya jika ukuran sesuai
        uploaded_data <- read.csv(input$file$datapath)
        data(uploaded_data)
      }
    }
  })
  
  # Tampilkan Data
  output$data_table <- renderDataTable({
    req(data())
    
    if(input$close_data_table){
      NULL
    }
    
    else{
      datatable(data(), options = list(scrollX = TRUE,
                                       scrollY = '200px'  # Set height of the table to make it scrollable
      )
      )
    }
  })
  
  
  # Output untuk memilih peubah respons
  output$select_response <- renderUI({
    req(data())  # Pastikan data tersedia
    
    # Filter variabel yang memiliki 2 nilai unik
    biner_vars <- names(data())[sapply(data(), function(x) {
      length(unique(na.omit(x))) == 2
    })]
    
    if (length(biner_vars) == 0) {
      # Jika tidak ada variabel biner, tampilkan pesan
      return(tags$div("Tidak ada peubah dengan dua nilai unik dalam data."))
    }
    
    # Dropdown untuk memilih variabel respon
    selectInput(
      "response_var", 
      "Pilih Peubah Respon (Biner)", 
      choices = sort(biner_vars)
    )
  })
  
  
  
  # Output untuk memilih peubah prediktor
  output$select_predictors <- renderUI({
    req(data(), input$response_var)
    # Hilangkan peubah yang dipilih sebagai respons dari daftar prediktor
    available_predictors <- sort(setdiff(names(data()), input$response_var))
    selectInput("predictor_vars", "Pilih Peubah Prediktor",
                choices = available_predictors, multiple = TRUE)
  })
  
  # Button
  observeEvent(input$remove_all, {
    updateSelectizeInput(session,"predictor_vars",choices=sort(setdiff(names(data()), input$response_var)), 
                         selected=NULL, options = list(placeholder="Pilih Setidaknya Satu Prediktor")
    )
  })
  observeEvent(input$add_all, {
    updateSelectizeInput(session,"predictor_vars",choices= sort(setdiff(names(data()), input$response_var)),
                         selected=names(data()))
  })
  
  # Jalankan model regresi logistik
  response_levels <- reactiveVal()
  
  model_result <- eventReactive(input$fit_model, {
    req(input$response_var, input$predictor_vars)
    
    model_data <- data()
    
    unique_values <- unique(model_data[[input$response_var]])
    response_levels(unique_values)  # Simpan ke reactiveVal
    
    model_data[[input$response_var]] <- ifelse(
      model_data[[input$response_var]] == unique_values[1], 0, 1
    )
    
    formula <- as.formula(
      paste(input$response_var, "~", paste(input$predictor_vars, collapse = "+"))
    )
    glm(formula, data = model_data, family = binomial)
  })
  
  
  
  
  # Logistic Regression Tab
  output$logistic_summary <- renderDT({
    req(model_result()) 
    logistic_model <- model_result()
    
    # Cek format koefisien
    exponentiate <- ifelse(input$coef_format == "odds_ratio", TRUE, FALSE)
    
    # Cek Tingkat Kepercayaan
    conf_level <- as.numeric(input$confidence_level)
    
    summary_table <- tidy_parameters(
      logistic_model,
      conf.int = TRUE,
      conf.level = conf_level,
      ci_method = "wald",
      exponentiate = exponentiate
    ) %>%
      mutate(across(where(is.numeric), ~ round(.x, 3)))  # Round to 2 decimals
    
    colnames(summary_table) <- c('Variabel', 'Dugaan Koef.', 'Galat Baku', 'Tingkat Kepercayaan', 'Batas Bawah', 'Batas Atas',
                                 'statistik','db', 'Nilai-p')
    summary_table <- summary_table[, -c(7, 8)]
    
    
    # Create the datatable with scrollable and expandable options
    datatable(summary_table,
              options = list(
                pageLength = 20,  # Set initial number of rows displayed
                scrollY = '380px',  # Set height of the table to make it scrollable
                scroller = TRUE,  # Enable scroller for expandable rows
                dom = 'Bfrtip',  # Allows the use of buttons (optional)
                searching = FALSE
              ),
              class = 'display',
              style = 'bootstrap',
              rownames = FALSE)
  })
  
  # Function to download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("regresi_logistik_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(model_result()) 
      logistic_model <- model_result()
      
      # Cek format koefisien
      exponentiate <- ifelse(input$coef_format == "odds_ratio", TRUE, FALSE)
      
      # Cek Tingkat Kepercayaan
      conf_level <- as.numeric(input$confidence_level)
      
      summary_table <- tidy_parameters(
        logistic_model, 
        conf.int = TRUE, 
        conf.level = conf_level,
        ci_method = "wald", 
        exponentiate = exponentiate
      ) %>% 
        mutate(across(where(is.numeric), ~ round(.x, 3)))  # Round to 2 decimals
      
      colnames(summary_table) <- c('Variabel', 'Dugaan Koef.', 'Galat Baku', 'Tingkat Kepercayaan', 'Batas Bawah', 'Batas Atas', 
                                   'statistik','db', 'Nilai-p')
      summary_table <- summary_table[, -c(7, 8)]
      
      write.csv(summary_table, file, row.names = FALSE)
    }
  )
  
  
  
  # Output Pemeriksaan Multikolinearitas
  output$vif_table <- renderDT({
    req(model_result(), input$predictor_vars)
    
    logistic_model <- model_result()
    
    # Periksa apakah checkbox multikol diaktifkan
    if (input$multikol) {
      # Periksa apakah lebih dari satu prediktor dipilih
      if (length(input$predictor_vars) > 1) {
        # Coba hitung VIF, pastikan model valid
        vif_calc <- tryCatch({
          vif(logistic_model)
        }, error = function(e) {
          NULL
        })
        
        # Validasi hasil VIF
        if (!is.null(vif_calc)) {
          if (class(vif_calc)[1] == "matrix") {
            # Jika terdapat setidaknya 1 prediktor kategorik
            vif_df <- data.frame(
              Variabel = row.names(vif_calc),
              GVIF = round(vif_calc[, 1], 2),
              row.names = NULL
            )
          } else {
            # Jika semua prediktor adalah numerik
            vif_df <- data.frame(
              Variabel = attr(terms(logistic_model), "term.labels"),
              VIF = round(vif_calc, 2)
            )
          }
        } else {
          vif_df <- data.frame(Peringatan = "Tidak dapat menghitung VIF. Periksa model.")
        }
        
      } else {
        # Jika kurang dari dua variabel dipilih
        vif_df <- data.frame(Peringatan = "Minimal pilih dua variabel prediktor.")
      }
    } else {
      # Jika checkbox tidak diaktifkan
      vif_df <- NULL
    }
    
    # Tampilkan tabel
    datatable(vif_df, 
              options = list(
                pageLength = 20,  # Set initial number of rows displayed
                scrollY = '180px',  # Set height of the table to make it scrollable
                scroller = TRUE,  # Enable scroller for expandable rows
                dom = 'Bfrtip',  # Allows the use of buttons (optional)
                searching = FALSE
              ),
              class = 'display',
              style = 'bootstrap',
              rownames = FALSE)
  })
  
  
  
  
  # Output Nilai AIC BIC Deviance
  output$aic_deviance <- renderPrint({
    req(model_result(), input$predictor_vars)
    
    if (input$eval_model){
      req(input$predictor_vars)  # Ensure predictors are selected
      
      logistic_model <- model_result()
      
      # Menghitung metrik model logistik
      model_metrics <- list(
        "AIC" = round(AIC(logistic_model), 2),
        "BIC" = round(BIC(logistic_model), 2),
        "Residual Deviance" = round(logistic_model$deviance, 2)
      )
      
      
      # Menyusun HTML untuk menampilkan metrik dalam bentuk card
      output_text <- ""
      for (metric_name in names(model_metrics)) {
        output_text <- paste0(output_text, 
                              "<div style='background-color: #0A3981; padding: 10px; margin: 10px 0; border-radius: 10px; color: white; font-family: \"Arial\", sans-serif; text-align: center;'>",
                              "<h4 style='font-size: 20px;'>", metric_name, "</h4>",
                              "<p style='font-size: 25px; font-weight: bold;'>", round(model_metrics[[metric_name]], 2), "</p>",
                              "</div>")
      }
      
      HTML(output_text)
      
    }
    
    
    
  })
  
  
  # Output Confusion Matrix (Buat Agar otomatis Ganti Value Variabel Respon)
  output$conf_matrix <- renderPlot({
    req(input$response_var, input$predictor_vars, model_result(), response_levels())
    
    
    if(input$conf_mat){
      req(input$predictor_vars)  # Ensure predictors are selected
      
      logistic_model <- model_result()
      response <- as.character(response_levels())
      #print(response[2])
      
      preds <- predict(logistic_model, type = "response")
      threshold <- 0.5
      predicted <- ifelse(preds < threshold, response[1], response[2])
      actual <- data()[[input$response_var]]
      
      
      actual <- factor(actual,
                       levels = c(response[1], response[2]))
      predicted <- factor(predicted,
                          levels = c(response[1], response[2]))
      
      
      # Menghasilkan confusion matrix
      cm <- confusionMatrix(predicted, actual)
      
      
      output$conf_matrix_acc <- renderUI({
        if(input$conf_mat & length(input$predictor_vars) > 0){
          
          cm_akurasi <- list(
            Accuracy = cm$overall['Accuracy'],
            Kappa = cm$overall['Kappa'],
            Sensitivity = cm$byClass['Sensitivity'],
            Specificity = cm$byClass['Specificity'],
            `Balanced Accuracy` = cm$byClass['Balanced Accuracy']
          )
          
          # Menyusun progress bar untuk menampilkan metrik
          left_cols <- ""
          right_cols <- ""
          progress_value <- ""
          
          for (i in seq_along(cm_akurasi)) {
            metric_name <- names(cm_akurasi)[i]
            progress_value <- round(cm_akurasi[[metric_name]] * 100) # Mengubah nilai menjadi persen
            progress_bar <- paste0(
              "<div style='margin: 10px 0;'>",
              "<h5 style='color: #2C3E50;'>", metric_name, "</h5>",
              "<div style='background-color: #ECF0F1; border-radius: 5px;'>",
              "<div style='background-color: #27AE60; width: ", progress_value, "%; height: 10px; border-radius: 5px;'></div>",
              "</div>",
              "<p style='font-size: 14px; font-weight: bold; color: #2C3E50;'>", progress_value, "%</p>",
              "</div>"
            )
            
            # Menyusun progress bar ke dalam kolom kiri atau kanan
            if (i <= 3) {
              left_cols <- paste0(left_cols, progress_bar)
            } else {
              right_cols <- paste0(right_cols, progress_bar)
            }
          }
          
          # Mengatur layout menjadi dua kolom hanya jika input$conf_mat TRUE
          output_text <- paste0(
            "<div style='display: flex; justify-content: space-between;'>",
            "<div style='width: 48%;'>", left_cols, "</div>",
            "<div style='width: 48%;'>", right_cols, "</div>",
            "</div>"
          )
          
          HTML(output_text)
        } else {
          # Jika input$conf_mat tidak dicentang, tidak menampilkan apa-apa
          HTML("")
        }
      })
      
      # Menyimpan confusion matrix dalam bentuk data frame
      conf_matrix_df <- as.data.frame(cm$table)
      conf_matrix_df$Amatan <- conf_matrix_df$Freq
      
      #  plot confusion
      p <- ggplot(conf_matrix_df, aes(x = Reference, y = Prediction, fill = Amatan)) +
        geom_tile(color = "white") +  # Membuat kotak dengan warna
        scale_fill_gradient(low = "#D4EBF8", high = "#1F509A") +  # Skala warna
        geom_text(aes(label = Amatan), color = "black", size = 5, fontface = "bold") +  # Menambahkan label teks
        theme_minimal(base_size = 16) +  # Menggunakan tema minimal dengan font lebih besar
        labs(
          title = 'Kinerja Model',
          x = "Aktual",
          y = "Prediksi"
        ) +   #coord_fixed(ratio=0.8) +  # Memastikan kotak tetap berbentuk persegi
        guides(fill = "none") +  # Menghapus colorbar
        theme(
          axis.text.x = element_text(angle = 0, hjust = 1, size = 16, family = "Arial", color = "#2C3E50"),  # Menyesuaikan font X axis
          axis.text.y = element_text(size = 16, family = "Arial", color = "#2C3E50"),  # Menyesuaikan font Y axis
          plot.title = element_text(size = 20, family = "Arial", face = "bold", color = "#2980B9"),  # Judul besar dan bold
          plot.subtitle = element_text(size = 16, family = "Arial", face = "italic", color = "#7F8C8D"),  # Subtitle dengan gaya italic
          plot.background = element_rect(fill = "#ECF0F1", color = "#BDC3C7", linewidth = 2)  # Latar belakang plot
        )
      p
      # grid.arrange(p,
      #              widths = unit(3, "in"),
      #              heights = unit(3, "in"))
    }
  })
  
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)


