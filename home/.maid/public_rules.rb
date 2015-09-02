Maid.rules do
  rule 'Clean my Desk' do
    move(
      dir('~/Desktop/*'),
      mkdir("~/Puffer")
    )
  end

  rule 'File away Taz' do
    move(
      dir('~/Downloads/taz_*{epub,txt,pdf}'),
      mkdir("~/Lager/eBooks/taz/")
    )
  end

  rule 'Keep Dear Mac OS Posts' do
    move(
      dir('~/Puffer/dearmacos*'),
      mkdir("~/Puffer/DearMacOS")
    )
    move(
      dir('~/Downloads/dearmacos*'),
      mkdir("~/Puffer/DearMacOS")
    )
  end

  rule 'Move PDF Files to be buffered' do
    move(
      dir('~/Puffer/*.pdf'),
      mkdir("~/Puffer/#{DateTime.now.strftime("%Y-%m")}/")
    )
    move(
      dir('~/Downloads/*.pdf'),
      mkdir("~/Puffer/#{DateTime.now.strftime("%Y-%m")}/")
    )
  end

  rule 'Move PDF Files to be buffered' do
    move(
      dir('~/Puffer/*.pdf'),
      mkdir("~/Puffer/#{DateTime.now.strftime("%Y-%m")}/")
    )
    move(
      dir('~/Downloads/*.pdf'),
      mkdir("~/Puffer/#{DateTime.now.strftime("%Y-%m")}/")
    )
  end

  rule 'Move Mediathek Downlads' do
    move(
      dir('~/*{nano,kulturzeit,Abenteuer_Forschung,hitec,Arte,ARD,ZDF,wido}*.{mkv,mp4}'),
      mkdir("~/Puffer/Videos/Mediathek/")
    )
  end

  rule 'Move Screenshots' do
    move(
      dir('~/{Screen,screen,screnn}*.{png,jpg,JPG,PNG}'),
      mkdir("~/Puffer/Bilder/Screenshots/")
    )
  end

  rule 'Move files away from Buffer' do
    move(
      dir('~/Puffer/*.{png,jpg,jpeg,tif,gif}'),
      mkdir("~/Puffer/Bilder/")
    )
  end

  rule 'Delete old downloads' do
    dir('~/Downloads/*.{deb,zip,tar.gz,gz,bz,bz2,7z,rpm}').each do |path|
      trash(path) if 3.days.since?(modified_at(path))
    end
  end

  rule 'Delete extracted folders' do
    dir('~/Downloads/*').each do |path|
      trash(path) if 5.days.since?(modified_at(path))
    end
  end

  rule 'Delete partial downloads' do
    dir('~/Puffer/*.part').each do |path|
      trash(path) if 1.days.since?(accessed_at(path))
    end
  end
end
