require 'fileutils'

Outdir = "./out"

def rename(prefix)
  oldfiles = Dir::glob("*.*")
  FileUtils.mkdir_p(Outdir) unless FileTest.exist?(Outdir)
  oldfiles.each{|f|
    newname = Outdir + "/" + prefix + "-" + f
    FileUtils.copy(f, newname)
    print("file [", f, "] is copied to [", newname, "]\n")
  }
end

if ARGV.length() < 1 then
  print("Usage:\n", "   > ruby scriptname.rb prefix\n")
else
  rename(ARGV[0])
end




