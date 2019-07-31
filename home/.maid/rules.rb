require_relative File.expand_path "~/.maid/private_rules.rb"

Maid.rules do
  NOW = DateTime.now.strftime("%Y-%m")
  rule 'File away Taz' do
    move(
      dir('~/Downloads/taz_*{epub,txt,pdf}'),
      mkdir("~/Lager/eBooks/taz/")
    )
  end

  rule 'Safe the Ergodox Layouts' do
    move(
      dir('~/Downloads/ergodox_ez*.hex'),
      mkdir("~/Lager/Ergodox-Layouts/")
    )
  end

  rule 'Move PDF Files to be buffered' do
    move(
      dir('~/Downloads/*.{PDF,pdf}'),
      mkdir("~/Lager/Downloaded-Documents/#{NOW}/")
    )
  end

  rule 'Move Screenshots into Month-Folders' do
    move(
      dir('~/Pictures/*Screenshot*png'),
      mkdir("~/Pictures/Screenshots/#{NOW}/")
    )
  end

  rule 'Move Mediathek Downlads' do
    move(
      dir('~/*{nano,kulturzeit,Abenteuer_Forschung,hitec,Arte,ARD,ZDF,wido}*.{mkv,mp4}'),
      mkdir("~/Videos/Mediathek/")
    )
  end

  rule 'Move files away from Buffer' do
    move(
      dir('~/Downloads/*.{png,jpg,jpeg,tif,gif,bmp,svg}'),
      mkdir("~/Lager/Downloaded-Pictures/#{NOW}/")
    )
  end

  rule 'Move ISOs' do
    dir('~/Downloads/*.iso').each do |f|
      name = File.basename(f, ".iso").capitalize
      move(f, mkdir("~/Lager/Images/#{name}/"))
    end
  end

  rule 'Delete old and partial downloads' do
    dir('~/Downloads/*.{deb,zip,nzb,bin,tgz,tar.gz,xz,gz,bz,jar,txz,dmg,exe,bz2,7z,rpm,part}').each do |path|
      trash(path) if 3.days.since?(modified_at(path))
    end
  end

  rule 'Delete extracted folders and really old shit' do
    dir('~/Downloads/.*').each do |path|
      puts path
      trash(path) if 5.days.since?(modified_at(path)) && File.directory?(path)
      trash(path) if 2.weeks.since?(modified_at(path))
    end
  end
end
