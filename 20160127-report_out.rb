# coding: utf-8
ID_FILE = "2015e2w3pc2.csv"
IDs = File.open(ID_FILE).readlines.map{|f| f.chomp}.sort()

Suffixs = {
  1 => ["reidai4_4b(sai)*.xlsx", "kadai4_5(sai)*.xlsx"],
  2 => ["reidai4_5a(sai)*.xlsx", "reidai4_5b(sai)*.xlsx", "kadai4_6(sai)*.xlsx"],
  3 => ["reidai4_5c(sai)*.xlsx", "reidai4_6(sai)*.xlsm", "kadai4_7a(sai)*.xlsx", "kadai4_7b(sai)*.xlsm"],
  4 => ["kadai4_8a(sai)*.xlsx", "kadai4_8b(sai)*.xlsm"],
#  5 => [],
  6 => ["vbreidai1(sai)*", "vbrenshu1_1(sai)*"],
  7 => ["vbreidai2(sai)*", "vbrenshu2_1(sai)*"],
  8 => ["vbreidai3(sai)*",  "vbrenshu3_1(sai)*"],
  9 => ["vbf1_1(sai)*", "vbf1_2(sai)*", "vbf1_3(sai)*", "vbf1(sai)*.docx"],
  10 => ["vbreidai4(sai)*", "vbrenshu4_1(sai)*"],
  11 => ["vbreidai5(sai)*", "vbrenshu5_1(sai)*"],
  12 => ["vbf2_1(sai)*", "vbf2_2(sai)*", "vbf2(sai)*.docx"],
  13 => ["vbf3_1(sai)*", "vbf3_2(sai)*", "vbf3(sai)*.docx"],
  14 => ["vbf4_1(sai)*", "vbf4_2(sai)*"],
  15 => ["vbweb(sai)*", "vbclock(sai)*"]
}


def check
  print("\\documentclass{jsarticle}")
  print("\\begin{document}")
  print("\\footnotesize\n")

  IDs.each do |id|
    print("\\section*{", id, "}\n")
    print("\\begin{itemize}\n")
    Suffixs.keys().sort().each do |no|
      print("\\item No.", no, "\n")
      submits = File.open("no"+format("%02d",no)+".txt").readlines.map{|f| f.chomp}

      print("\\begin{enumerate}\n")
      
      Suffixs[no].each do |suffix|
        kadaiReg = "^" + id + suffix + "$"
        print("\\item \\verb|", suffix, "| :")
        files = submits.select{|f| f.match(kadaiReg)}
        if files.empty?() then
          print(" X")
        else
          print(" OK")
        end
        files.each do |file|
          print("\\verb| ", file, "|")
        end
        print("\n")
      end

      print("\\end{enumerate}\n")


    end
    print("\\end{itemize}\n")
    print("\\newpage\n")
  end
  print("\\end{document}")

end

check
