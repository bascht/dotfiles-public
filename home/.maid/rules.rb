require_relative File.expand_path "~/.maid/private_rules.rb"

Maid.rules do
  NOW = DateTime.now.strftime("%Y-%m")
  rule 'File away Taz' do
    move(
      dir('~/Downloads/taz_*{epub,txt,pdf}'),
      mkdir("~/Lager/eBooks/taz/")
    )
  end

  rule 'Move PDF Files to be buffered' do
    move(
      dir('~/Downloads/*.{PDF,pdf}'),
      mkdir("~/Lager/Downloaded-Documents/#{NOW}/")
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
      dir('~/Downloads/*.{png,jpg,jpeg,tif,gif}'),
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
    dir('~/Downloads/*.{deb,zip,tgz,xz,gz,bz,jar,txz,dmg,exe,bz2,7z,rpm,part}').each do |path|
      trash(path) if 3.days.since?(modified_at(path))
    end
  end

  rule 'Delete extracted folders' do
    dir('~/Downloads/*').each do |path|
      trash(path) if 5.days.since?(modified_at(path)) && File.directory?(path)
    end
  end

end
