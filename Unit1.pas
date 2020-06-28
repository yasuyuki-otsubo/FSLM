unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.ImageList,
  FMX.ImgList, System.IOUtils, FMX.ListBox;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    Memo1: TMemo;
    btnExit: TSpeedButton;
    ImageList1: TImageList;
    Splitter1: TSplitter;
    btnClear: TSpeedButton;
    cbSiteId: TComboBox;
    procedure Panel2DragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure Panel2DragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure btnExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnClearClick(Sender: TObject);
    procedure cbSiteIdChange(Sender: TObject);
  private
    { private 宣言 }
    FLog : TStringList;
    //
    procedure LoadLogFile(filepath : string);
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnClearClick(Sender: TObject);
begin
  FLog.Clear;
  Memo1.Lines.Add('総登録行数:'+IntToStr(FLog.Count));
end;

procedure TForm1.btnExitClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm1.cbSiteIdChange(Sender: TObject);
var
  FList : TStringList;
  ii : Integer;
begin
  if cbSiteId.ItemIndex>0 then
  begin
    Memo1.Lines.Add(cbSiteId.Items[cbSiteId.ItemIndex]);
    FList := TStringList.Create;
    try
      FList.BeginUpdate;
      for ii := 0 to FLog.Count - 1 do
      begin
        if (FLog[ii].IndexOf('Sites-'+cbSiteId.Items[cbSiteId.ItemIndex]+'-Site')> 0) or
           (FLog[ii].IndexOf('/s/'+cbSiteId.Items[cbSiteId.ItemIndex]+'/dw/')> 0)  then
        begin
          FList.Add(FLog[ii]);
        end;
      end;
    finally
      FList.EndUpdate;
    end;
    //
    FList.SaveToFile('filtered.txt', TEncoding.Default);
    FList.Clear;
    FList.Free;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(Flog) then
  begin
    FLog.Clear;
    FLog.Free;
  end;
  //
  Memo1.Lines.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not Assigned(FLog) then
  begin
    FLog := TStringList.Create;
  end;
  //
  Memo1.Lines.Add('登録行数:'+IntToStr(FLog.Count))
end;

{
procedure TForm1.LoadLogFile(filepath: string);
var
  FReplacer : TFileSearchReplace;
begin
  FReplacer := TFileSearchReplace.Create(filepath);
  try
    // -----------------------------------------
    //  ログ情報を１件、１行に返還する
    // -----------------------------------------
    //
    // LF を一旦全て削除する
    FReplacer.Replace('','',[rfReplaceAll]);
    //
    // パターンにより改めて LF を設定する
    FReplacer.Replace('','',[rfReplaceAll]);
  finally
    FReplacer.Free;
  end;
end;

procedure TForm1.LoadLogFile2(filepath: string);
var
  FReplacer : TStringList;
begin
  FReplacer := TStringList.Create;
  try
    FReplacer.LoadFromFile(filepath, TEncoding.UTF8);
    FReplacer.Text.Replace('\n', '=LF;', [rfReplaceAll]);
    FReplacer.SaveToFile('nolf.txt');
    FReplacer.Text.Replace('=LF;[', '\n[', [rfReplaceAll]);
    FReplacer.SaveToFile('newlf.txt');
    self.FLog.Append(FReplacer.Text);
    Memo1.Lines.Add('登録行数:'+IntToStr(FLog.Count))
  finally
    FReplacer.Free;
  end;
end;
}

procedure TForm1.LoadLogFile(filepath: string);
var
  FReplacer : string;
  FStr : string;
  FList : TStringList;
begin
  try
    //
    // ドラッグされたファイルを全て読む
    FReplacer := TFile.ReadAllText(filepath, TEncoding.UTF8);
    //
    // 一旦 LF改行を取り除く
    FStr := StringReplace(FReplacer, #10, '=LF;', [rfReplaceAll]);
    FList := TStringList.Create;
    FList.Add(FStr);
    FList.SaveToFile('nolf.txt');
    //
    // 全てタイムスタンプが先頭になるように改行を挿入する
    //FReplacer := StringReplace(FStr, '=LF;[', #10'[', [rfReplaceAll]); // LF
    FReplacer := StringReplace(FStr, '=LF;[', #13#10'[', [rfReplaceAll]);
    FList.Clear;
    FList.Add(FReplacer);
    FList.SaveToFile('tmp.txt');
    //
    // 不要なログ情報を削除する
    FList.Clear;
    FList.LoadFromFile('tmp.txt', TEncoding.Default);
    if FList.Count>0 then
    begin
      var ii : Integer;
      //var tmp: String;
      for ii := (FList.Count - 1) downto 0 do
      begin
        if (FList[ii].IndexOf('RepeatedMessageSuppressingFilter')>0) then
        begin
          FList.Delete(ii);
        end;
      end;
    end;
    FList.SaveToFile('newlf2.txt');
    //
    // ログファイルをマージする
    FLog.AddStrings(FList);
    FLog.Sort;
    FLog.SaveToFile('sorted.txt', TEncoding.Default);
    //
    Memo1.Lines.Add('登録行数:'+IntToStr(FList.Count));
    //Memo1.Lines.AddStrings(FList);
    Memo1.Lines.Add('総登録行数:'+IntToStr(FLog.Count));
  finally
    FReplacer := '';
  end;
end;

procedure TForm1.Panel2DragDrop(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
var
  ii: Integer;
begin
  if Length(Data.Files) > 0 then
  begin
    //Memo1.Lines.Clear;
    Memo1.BeginUpdate;
    for ii := 0 to (Length(Data.Files) - 1) do
    begin
      Memo1.Lines.Add(Data.Files[ii]);
      //
      LoadLogFile(Data.Files[ii]);
    end;
    Memo1.EndUpdate;
  end;
end;

procedure TForm1.Panel2DragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
    Operation := TDragOperation.Copy;
end;

end.
