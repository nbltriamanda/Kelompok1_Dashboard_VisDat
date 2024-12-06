<p align="center" width="100%">
    <img width="100%" src="https://github.com/nbltriamanda/Kelompok1_Dashboard_VisDat/blob/main/Liburan%20Banner.png">
</p>

<div align="center">


# Dashboard Preferensi Liburan Pengunungan VS Pantai


[Deskripsi Dashboard](#film_projector-deskripsi-project) • [Tools](#tools-dataset) • [Dataset](#pencil-dataset) • [Metode dan Visualisasi Data](#chart_with_upwards_trend-visualisasi-data) • [Tim Pengembang](#woman_technologist-pengembang)

</div>

---

## :film_projector: **Deskripsi Dashboard**

Aplikasi ini bertujuan untuk menganalisis preferensi liburan pengguna menggunakan teknik statistik canggih, termasuk regresi logistik, analisis korelasi, ANOVA, dan uji chi-square. Dashboard ini fokus pada perbandingan preferensi liburan antara pantai dan pegunungan, serta menggali faktor-faktor yang mempengaruhi pilihan tersebut.

- **Overview:** Gambaran umum dari data liburan.
- **Exploratory Data Analysis (EDA):** Menyediakan berbagai visualisasi untuk mengeksplorasi hubungan antar variabel.
- **Regresi Logistik:** Model regresi logistik untuk memahami faktor yang mempengaruhi preferensi liburan (baik dari dataset dashboard maupun data yang diunggah pengguna).

Dengan visualisasi interaktif, pengguna dapat mengeksplorasi variabel seperti anggaran, cuaca, dan aktivitas favorit, serta menganalisis tren yang muncul. Aplikasi ini membantu pengguna memahami faktor-faktor yang memengaruhi keputusan liburan dan memberikan rekomendasi yang lebih akurat berdasarkan preferensi mereka.

🎯 **Dashboard Interaktif dapat diakses pada tautan ini!**  
[🌐 Visualisasi Dashboard - ShinyApps](https://srgr.shinyapps.io/EVD1/)

## 🧰 **Tools yang digunakan**
- 📈 **R Shiny** untuk visualisasi data interaktif.
- 📊 **R Studio** untuk analisis data.
- 🔍 Klasifikasi Preferensi liburan seseorang antara Gunung dan Pantai berdasarkan karakteristiknya

## 📑 **Dataset**
### 📋 **Tentang Dataset**
Dataset ini digunakan untuk menggali preferensi publik antara dua jenis liburan: pegunungan dan pantai. Melalui analisis mendalam terhadap data demografi dan gaya hidup, pengguna dapat menemukan pola unik yang memengaruhi pilihan liburan.

📂 **Sumber Dataset**:  
Dataset ini diambil dari Kaggle, tersedia di tautan berikut:  
[ Mountains Vs Beach Preference on Kaggle](https://www.kaggle.com/datasets/jahnavipaliwal/mountains-vs-beaches-preference)

### 🔢 **Variabel dalam Dataset**
Dataset ini memiliki Jumlah Data (Baris) sebanyak 52444 dengan Jumlah Variabel sebanyak 13 variabel yang ditampilkan pada tabel berikut!

|       Nama Variabel      |                       Tipe Data                              |                       Keterangan                          |
|:-------------------------|:-------------------------------------------------------------|:----------------------------------------------------------|
| Jenis Kelamin            | Kategorik (laki-laki, perempuan, non-biner)                  | Identitas gender individu                                 |
| Penghasilan              | Numerik                                                      | Pendapatan tahunan individu.                              |
| Pendidikan               | Kategorik (SMA, sarjana, master, doktor)                     | Tingkat pendidikan tertinggi yang dicapai                 |
| Frekuensi Liburan        | Numerik                                                      | Jumlah liburan yang diambil per tahun                     |
| Aktivitas yang Disukai   | Kategorik (hiking, berenang, ski, berjemur)                  | Aktivitas yang disukai individu saat liburan              |
| Anggaran Liburan         | Numerik                                                      | Anggaran yang dialokasikan untuk liburan                  |
| Lokasi                   | Kategorik (perkotaan, pinggiran kota, pedesaan)              | Jenis tempat tinggal                                      |
| Jarak ke Pegunungan      | Numerik                                                      | Jarak dari pegunungan terdekat (dalam mil)                |
| Jarak ke Pantai          | Numerik                                                      | Jarak dari pantai terdekat (dalam mil)                    |
| Musim Favorit            | Kategorik (musim panas, musim dingin, musim semi, gugur)     | Musim yang disukai untuk liburan                          |
| Pemilik Hewan Peliharaan | 0 = Tidak, 1 = Ya                                            | Apakah individu memiliki hewan peliharaan.                |
| Sensitivitas Lingkungan  | 0 = Tidak, 1 = Ya                                            | Apakah individu memiliki kekhawatiran terhadap lingkungan |
## 🔍 **Metode Analisis**
### 📈 **Regresi Logistik**
Regresi logistik adalah metode regresi yang digunakan untuk memodelkan hubungan antara satu variabel dependen kategorikal dengan satu atau lebih variabel independen. Biasanya, variabel dependen yang digunakan dalam regresi logistik adalah variabel biner (dua kategori), seperti ya/tidak, sukses/gagal, atau lainnya
<p align="center" width="100%">
    <img width="100%" src="https://github.com/nbltriamanda/Kelompok1_Dashboard_VisDat/blob/main/LogReg_1.png">
</p>
    
**1. Fungsi Logistik (Sigmoid)**
Regresi logistik menggunakan fungsi logistik atau sigmoid, yang mengubah output linear menjadi probabilitas dalam rentang antara 0 dan 1. Dalam regresi logistik, fungsi ini membentuk kurva S dan menentukan nilai ambang batas (threshold) untuk memutuskan probabilitas 0 atau 1. Nilai di atas ambang batas cenderung ke 1, sementara nilai di bawah ambang batas cenderung ke 0.

**2. Model Regresi Logistik**
Dalam regresi logistik, koefisien model menggambarkan perubahan pada log-odds untuk setiap unit perubahan pada prediktor. Jika koefisien positif, maka odds (kemungkinan) suatu peristiwa terjadi akan meningkat dengan bertambahnya nilai prediktor. Sebaliknya, koefisien negatif menunjukkan bahwa odds suatu peristiwa terjadi akan berkurang.
Koefisien dalam model regresi logistik dapat diubah menjadi odds-ratio (OR) dengan menghitung exp(koefisien).
Persamaan log-odds regresi logistik: log(P(Y=1|X) / (1 - P(Y=1|X))) = b0 + b1*X1 + b2*X2 + ... + bn*Xn
Untuk mengubah koefisien menjadi odds ratio, gunakan rumus: OR = exp(b1), OR = exp(b2), ... , OR = exp(bn)

**3. Asumsi Regresi Logistik**
Variabel dependen harus bersifat kategorikal (misalnya, dua kelas: 0 atau 1).
Tidak boleh ada multikolinearitas antara variabel independen. Artinya, variabel independen seharusnya tidak saling berkorelasi tinggi.

---

## 🫂 : Tim Pengembang
💡 **Kelompok 1** terdiri dari:

- **👨 [Indra Rivaldi Siregar]([https://github.com/insersir])**  

- **🧕 [Erdanisa Aghnia Ilmani ]([https://github.com/erdanisaaghnia])**  

- **🧕🏻 [Nabila Tri Amanda]([https://github.com/nbltriamanda])**  
