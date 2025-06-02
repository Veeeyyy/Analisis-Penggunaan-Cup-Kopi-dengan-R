# Memanggil library shiny untuk membuat aplikasi web interaktif
library(shiny)

# Menentukan perkiraan berat cup kopi, waktu terurai, dan tingginya
berat_cup <- 15  # gram per cup
waktu_urai <- 20  # tahun per cup untuk terurai
tinggi_cup <- 10 #tinggi per cup (dalam cm)

# UI
ui <- fluidPage(
  
  # Menambahkan CSS untuk styling tampilan (background coklat, teks putih, dsb)
  tags$head(
    tags$style(HTML("
      body {
        background-color: #4b2e2e;  /* warna coklat kopi */
        color: white;               /* teks putih */
        font-family: Arial, sans-serif;
      }
      .input-panel {
        background-color: rgba(255, 255, 255, 0.1);
        padding: 20px;
        border-radius: 10px;
      }
      .output-panel {
        background-color: rgba(255, 255, 255, 0.1);
        padding: 20px;
        border-radius: 10px;
      }
      .emot-box {
        font-size: 30px;
        line-height: 1;
      }
      .btn-download {
        margin-top: 15px;
        background-color: #40282c;
        color: white;
        border: none;
        padding: 10px;
        border-radius: 5px;
      }
    "))
  ),
  
  # Membagi halaman menjadi dua kolom: input (4 kolom), output (8 kolom)
  fluidRow(
    br(),
    column(4,  # Kolom input sisi kiri
           div(class = "input-panel",
               
               # Input jumlah cup kopi dalam minggu
               numericInput("kopi_per_minggu", "Konsumsi kopi-- (rata-rata atau kira-kira aja biasanya berapa. contoh: (3) cup)", value = 0, min = 0),
               
               # Input durasi (minggu)
               numericInput("minggu", "--dalam ... minggu (konsumsinya per berapa minggu, contoh: per (2) minggu)", value = 0, min = 0, max = 4),
               
               # Input frekuensi pakai tumbler
               numericInput("frekuensi_tumbler", "Frekuensi pemakaian tumbler dalam konsumsi kopi per minggu", value = 0, min = 0),
               
               # Input jumlah teman yang ikut
               numericInput("teman", "Jumlah teman yang ikut berpartisipasi (misalkan atau kira-kira saja)", value = 0, min = 0),
               br(),
               
               # Tombol untuk mengunduh file hasil analisis
               downloadButton("download_csv", "Unduh Hasil Analisis*", class = "btn-download"),
               br(),
               br(),
               # Input untuk mengunggah file CSV
               #fileInput("file_csv", "Unggah hasil analisis*")
               
               
               # Tautan ke Google Form
               tags$a(href = "https://forms.gle/CDhMw6DY1TpT52Lm7", 
                      "Upload hasil yang baru kamu download ke sini*", 
                      target = "_blank", 
                      style = "color: #ffcc99; font-weight: bold; text-decoration: underline;"),
               
               # Ucapan terima kasih
               #uiOutput("feedback_output")
           )
    ),
    
    
    column(8,  # Kolom output sisi kanan
           div(class = "output-panel",
               
               # Judul
               h3("Hasil Analisis Sampah Kopi Kamu"),
               
               # Teks analisis dinamis
               uiOutput("teks_output"),
               
               # Grafik perbandingan
               plotOutput("grafik"),
           )
    )
  )
)

# Bagian server (logika dan perhitungan)
server <- function(input, output) {
  
  # Fungsi reaktif: menghitung hasil dari input user
  data_hasil <- reactive({
    
    cup_aktual <- input$kopi_per_minggu * input$minggu                # total cup dipakai
    tumbler_pengganti <- input$frekuensi_tumbler * input$minggu      # total tumbler digunakan
    cup_tersisa <- max(0, cup_aktual - tumbler_pengganti)            # cup yang benar-benar dibuang
    tinggi_tumpukan <- cup_aktual * tinggi_cup            #tinggi tumpukan cup
    
    berat_total <- cup_tersisa * berat_cup                           # berat total sampah kopi
    urai_total <- cup_tersisa * waktu_urai                        # total waktu urai dengan asumsi terurai satu-persatu
    total_teman <- input$teman * cup_tersisa                         # dampak kalau teman ikut
    
    minggu_Mhsw <- input$minggu * 4
    
    # Mengembalikan semua nilai sebagai list
    list(
      cup = cup_tersisa,
      berat = berat_total,
      urai = urai_total,
      tinggi = tinggi_tumpukan,
      tumbler_efek = tumbler_pengganti,
      teman = total_teman,
      minggu = minggu_Mhsw
    )
  })
  
  
  
  # Menyimpan data survei ke file CSV lokal (otomatis)
  observeEvent({
    input$kopi_per_minggu
    input$minggu
    input$frekuensi_tumbler
    input$teman
  }, {
    hasil <- data_hasil()
    
    # Buat data frame dengan data yang diinput dan hasil perhitungan
    data_baru <- data.frame(
      Waktu = Sys.time(),  # waktu pengisian
      Kopi_per_minggu = input$kopi_per_minggu,
      Minggu = input$minggu,
      Frekuensi_tumbler = input$frekuensi_tumbler,
      Jumlah_teman = input$teman,
      Cup_dibuang = hasil$cup,
      Berat_sampah_gram = hasil$berat,
      Tahun_urai = hasil$urai,
      Cup_dikurangi_tumbler = hasil$tumbler_efek,
      Cup_dihasilkan_teman = hasil$teman
    )
    
    # Cek apakah file sudah ada
    file_path <- "data_survei.csv"
    if (!file.exists(file_path)) {
      # Kalau belum ada, buat file baru dengan header
      write.csv(data_baru, file_path, row.names = FALSE)
    } else {
      # Kalau sudah ada, tambahkan baris baru (append)
      write.table(data_baru, file_path, sep = ",", row.names = FALSE,
                  col.names = FALSE, append = TRUE)
    }
  })
  
  
  
  
  
  # Menampilkan teks analisis ke layar
  output$teks_output <- renderUI({
    hasil <- data_hasil()
    HTML(paste0(
      "<p>Dalam ", input$minggu, " minggu, kamu menghasilkan <b>", hasil$cup, " sampah cup kopi</b>.</p>",
      "<p>Dengan berat total <b>", hasil$berat, " gram</b></p>",
      "<p>Jika dibiarkan, butuh total <b>", hasil$urai, " tahun</b> untuk terurai.</p>",
      "<p>Sampah kopimu setara dengan tinggi tumpukan: <b>", hasil$tinggi, " cm</b>.</p>",
      "<p>Jika kamu pakai tumbler sebanyak <b>", hasil$tumbler_efek, " kali</b>, kamu bisa mengurangi <b>", hasil$tumbler_efek, " cup</b>.</p>",
      if (hasil$teman > 0) paste0("<p>Jika kamu ajak ", input$teman, " teman, bisa hemat total <b>", hasil$teman, " cup</b>!</p>") else ""
    ))
  })
  
  # Grafik batang: kamu vs rata-rata
  output$grafik <- renderPlot({
    hasil <- data_hasil()
    barplot(
      height = c(hasil$cup, hasil$minggu),            #4 dari survey sebelumnya
      names.arg = c("Kamu", "Rata-rata Mahasiswa Lain"),
      col = c("#cc9966", "#888888"),
      ylab = "Jumlah Cup",
      main = "Perbandingan Sampah Kopi Kamu dan Rata-Rata Sampah Kopi Mahasiswa Lainnya"
    )
  })
  
  # Ucapan terima kasih personal
  output$feedback_output <- renderUI({
    HTML("<br><h4>Terima kasih!</h4><p>Kamu sudah selangkah lebih dekat jadi bagian dari solusi, bukan polusi ✨</p> <p>Bawa tumbler sendiri — sayang uang dan lingkungan ^^ 🌍</p>")
  })
  
  
  
  
  # Handler untuk mengunduh CSV hasil analisis
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("hasil_analisis_sampah_kopi.csv")
    },
    content = function(file) {
      hasil <- data_hasil()
      write.csv(data.frame(
        Jumlah_Cup = hasil$cup,
        Berat_Sampah_gram = hasil$berat,
        Tahun_Urai = hasil$urai,
        Cup_Dikurangi_dengan_Tumbler = hasil$tumbler_efek,
        Cup_Dihasilkan_Teman = hasil$teman
      ), file, row.names = FALSE)
    }
  )
  
  
  
}

# Menjalankan aplikasi shiny
shinyApp(ui, server)
