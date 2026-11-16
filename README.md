# AdvancedResearchAnalytics v1.1.4 (candidate)

## Instalasi Windows / RStudio

1. Pastikan R versi 4.2 atau lebih baru, koneksi internet, serta akses instalasi paket R.
2. Unduh dan jalankan satu berkas `install_and_run.R` lewat `source("install_and_run.R")`.
3. Untuk penggunaan berikutnya, jalankan `AdvancedResearchAnalytics::run_app()`.

Alternatif untuk GitHub: `remotes::install_github("asriadi190197-stack/AdvancedResearchAnalytics", dependencies = NA)`.

PENTING: Skrip pemasangan GitHub di atas memakai branch `main` atau `master`. Agar versi
revisi yang diuji terpasang, unggah semua isi direktori paket ini ke root repo dahulu.

Jika `Application Control policy has blocked this file`, kode R tidak dapat mengabaikan
kebijakan Windows. Administrator perangkat perlu menyetujui instalasi dependensi R.
Tidak disarankan mematikan fitur keamanan.

Pengujian yang diperlukan sebelum rilis umum: R CMD check di Windows/Linux,
instalasi pada R bersih, uji setiap modul (HLM, IPD, SEM, Multivariate, ekspor),
dan kebijakan dependensi Windows. ZIP ini adalah paket sumber, **bukan** biner Windows.

Catatan: informasi email maintainer dan hak distribusi di `LICENSE` belum disahkan.
Pemegang hak cipta harus mengisi email sah dan menetapkan lisensi sebelum publikasi.
