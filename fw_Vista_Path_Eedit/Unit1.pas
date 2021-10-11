unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, FWVistaPathEdit;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    Memo1: TMemo;
    VistaPathEdit: TFWVistaPathEdit;
    procedure VistaPathEditAutoComplete(Sender: TObject; const Path: string;
      AutoComplete: TStrings);
    procedure VistaPathEditHasFolders(Sender: TObject; const Path: string;
      var HasFolders: Boolean);
    procedure VistaPathEditNewPath(Sender: TObject; const Path: string;
      var Accept: Boolean);
    procedure VistaPathEditQueryFolders(Sender: TObject; const Path: string;
      Data: TFoldersData);
    procedure VistaPathEditQueryHistory(Sender: TObject; Data: TFoldersData);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//  Первоначальная инициализация
// =============================================================================
procedure TForm1.FormCreate(Sender: TObject);
begin
  VistaPathEdit.Text := ExtractFilePath(ParamStr(0));
end;

//  Обработчик режима AutoComplete
//  В данном режиме запрашиваются подпапки в текущем каталоге,
//  наименование которых совпадает с введенным пользователем данными
//  Или список подпапок если пользователь сам вызвал
//  меню автокомплита комбинацией CTRL + пробел
// =============================================================================
procedure TForm1.VistaPathEditAutoComplete(Sender: TObject; const Path: string;
  AutoComplete: TStrings);
const
  NameSize = 4;
  VolumeCount = 26;
  TotalSize = NameSize * VolumeCount;
var
  SR: TSearchRec;
  APath, SubPath: string;
  Buff, Volume: String;
  I, Count: Integer;
begin
  // Смотрим пуст ли путь?
  if Path = '' then
  begin
    // Если пуст, то возвращаем список логических дисков в системе
    SetLength(Buff, TotalSize);
    Count := GetLogicalDriveStrings(TotalSize, @Buff[1]) div NameSize;
    for I := 0 to Count - 1 do
    begin
      Volume := PChar(@Buff[(I * NameSize) + 1]);
      AutoComplete.Add(ExcludeTrailingPathDelimiter(Volume));
    end;
    Exit;
  end;
  // В противном случае перебираем все папки и добавляем в список те,
  // имя котороых начинается с текста, введенного пользователем 
  APath := ExtractFilePath(Path);
  SubPath := LowerCase(ExtractFileName(Path));
  if FindFirst(APath + '*.*', faAnyFile, sr) = 0 then
  try
    repeat
      if SR.Name <> '.' then
        if SR.Name <> '..' then
        begin
          if SubPath <> '' then          
            if LowerCase(Copy(SR.Name, 1, Length(SubPath))) <> SubPath then
              Continue;
          if DirectoryExists(APath + SR.Name) then
            AutoComplete.Add(SR.Name);
        end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

//  Данный обработчик вызывается при создании нового таба.
//  Параметр HasFolders отвечает за отрисовку треугольника,
//  раскрывающего меню подпапок.
//  Соответственно задача обраотчика состоит в определение -
//  имеются ли в наличии в данном каталоге подкаталоги
// =============================================================================
procedure TForm1.VistaPathEditHasFolders(Sender: TObject; const Path: string;
  var HasFolders: Boolean);
var
  SR: TSearchRec;
  APath: string;
begin
  APath := IncludeTrailingPathDelimiter(Path);
  HasFolders := False;
  if FindFirst(APath + '*', faDirectory, sr) = 0 then
  try
    repeat
      if SR.Name <> '.' then
        if SR.Name <> '..' then
          if DirectoryExists(APath + SR.Name) then
          begin
            HasFolders := True;
            Break;
          end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

//  Обработчик вызывается в тот момент,
//  когда пользователь выбрал какой либо каталог.
//  Параметр Accept выставленный в False позволяет отменить выбор каталога.
// =============================================================================
procedure TForm1.VistaPathEditNewPath(Sender: TObject; const Path: string;
  var Accept: Boolean);
begin
  Caption := Format('FWVistaPathEdit demo [%s]', [Path]);;
end;

//  Обработчик вызывается в тот момент,
//  когда пользователь щелкает на треугольнике, раскрывающем меню.
//  Задача обработчика - передать список элементов открываемого меню.
//  В данном случае это список подкаталогов в текущем каталоге.
// =============================================================================
procedure TForm1.VistaPathEditQueryFolders(Sender: TObject; const Path: string;
  Data: TFoldersData);
var
  SR: TSearchRec;
  APath: string;
const
  NameSize = 4;
  VolumeCount = 26;
  TotalSize = NameSize * VolumeCount;
var
  Buff, Volume: String;
  I, Count: Integer;
begin
  if Path = '' then
  begin
    SetLength(Buff, TotalSize);
    Count := GetLogicalDriveStrings(TotalSize, @Buff[1]) div NameSize;
    for I := 0 to Count - 1 do
    begin
      Volume := PChar(@Buff[(I * NameSize) + 1]);
      Data.Add(MakeFoldersInfo(ExcludeTrailingPathDelimiter(Volume), 1));
    end;
    Exit;
  end;
  APath := IncludeTrailingPathDelimiter(Path);
  if FindFirst(APath + '*', faDirectory, sr) = 0 then
  try
    repeat
      if SR.Name <> '.' then
        if SR.Name <> '..' then
          if DirectoryExists(APath + SR.Name) then
            Data.Add(MakeFoldersInfo(SR.Name, 2));
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

//  Обработчик вызывается когда пользователь щелкает на меню комбобокса,
//  ответственного за отображение истории посешение.
//  Задача обработчика заполнить список истории валидной информацией.
// =============================================================================
procedure TForm1.VistaPathEditQueryHistory(Sender: TObject; Data: TFoldersData);
begin
  Data.Add(MakeFoldersInfo('c:\test1\qweqwe\', 3));
  Data.Add(MakeFoldersInfo('c:\test2\qweqwe\', 4));
  Data.Add(MakeFoldersInfo('c:\test3\qweqwe\', 5));
  Data.Add(MakeFoldersInfo('c:\test4\qweqwe\', 6));
end;

end.
