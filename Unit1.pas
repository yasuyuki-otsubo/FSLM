unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.ImageList,
  FMX.ImgList, System.IOUtils, FMX.ListBox, FMX.Objects,
  FMX.Platform, System.Rtti;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    btnExit: TSpeedButton;
    ImageList1: TImageList;
    Splitter1: TSplitter;
    btnClear: TSpeedButton;
    cbSiteId: TComboBox;
    btnCopy: TSpeedButton;
    RoundRect1: TRoundRect;
    Label1: TLabel;
    procedure Panel2DragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure Panel2DragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure btnExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnClearClick(Sender: TObject);
    procedure cbSiteIdChange(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
    { private 宣言 }
    FRawLog : TStringList;
    //
    procedure LoadLogFile(filepath : string);
    function GetFilteredLogText(filter: string): string;
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnClearClick(Sender: TObject);
begin
  FRawLog.Clear;
  Memo1.Lines.Add('総登録行数:'+IntToStr(FRawLog.Count));
end;

procedure TForm1.btnCopyClick(Sender: TObject);
var
  Clipboard: IFMXClipboardService;
  CopyText: string;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Clipboard) then
  begin
    if cbSiteId.ItemIndex>0 then
    begin
      CopyText := self.GetFilteredLogText(cbSiteId.Items[cbSiteId.ItemIndex]);
    end
    else
    begin
      CopyText := FRawLog.Text;
    end;
    Clipboard.SetClipboard(CopyText);
  end;
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
    FList.Add(GetFilteredLogText(cbSiteId.Items[cbSiteId.ItemIndex]));
    //
    if not Trim(FList.Text).IsEmpty then
    begin
      FList.SaveToFile('filtered.txt', TEncoding.Default);
    end
    else
    begin
      // モーダルダイアログの表示
      // FMX.Dialogs.ShowMessage('No log line !. Check SITE-ID please.');
    end;
    FList.Clear;
    FList.Free;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FRawLog) then
  begin
    FRawLog.Clear;
    FRawLog.Free;
  end;
  //
  Memo1.Lines.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not Assigned(FRawLog) then
  begin
    FRawLog := TStringList.Create;
  end;
  //
  Memo1.Lines.Add('登録行数:'+IntToStr(FRawLog.Count))
end;

function TForm1.GetFilteredLogText(filter: string): string;
var
  FList : TStringList;
  ii : Integer;
begin
  FList := TStringList.Create;
  try
    FList.BeginUpdate;
    for ii := 0 to FRawLog.Count - 1 do
    begin
      if (FRawLog[ii].IndexOf('Sites-'+filter+'-Site')>= 0) or
         (FRawLog[ii].IndexOf('/s/'+filter+'/dw/')>= 0)  then
      begin
        FList.Add(FRawLog[ii]);
      end;
    end;
  finally
    FList.EndUpdate;
  end;
  //
  //FList.SaveToFile('_filtered.txt', TEncoding.Default);
  Result := FList.Text;
  FList.Clear;
  FList.Free;
end;

procedure TForm1.LoadLogFile(filepath: string);
var
  FReplacer : string;
  FStr : string;
  FList : TStringList;
  ii : Integer;
begin
  try
    //
    // ドラッグされたファイルを全て読む
    FReplacer := TFile.ReadAllText(filepath, TEncoding.UTF8);
    // -----------------------------------------
    //  ログ情報を１件、１行に返還する
    // -----------------------------------------
    //
    // 一旦 LF改行を取り除く
    FStr := StringReplace(FReplacer, #10, '=LF;', [rfReplaceAll]);
    FList := TStringList.Create;
    //FList.Add(FStr);
    //FList.SaveToFile('nolf.txt');
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
      for ii := (FList.Count - 1) downto 0 do
      begin
        if (FList[ii].IndexOf('RepeatedMessageSuppressingFilter')>0) then
        begin
          FList.Delete(ii);
        end;
      end;
    end;
    //FList.SaveToFile('newlf2.txt');
    //
    // ログファイルをマージする
    FRawLog.AddStrings(FList);
    FRawLog.Sort;
    FRawLog.SaveToFile('sorted.txt', TEncoding.Default);
    //
    Memo1.Lines.Add('登録行数:'+IntToStr(FList.Count));
    //Memo1.Lines.AddStrings(FList);
    Memo1.Lines.Add('総登録行数:'+IntToStr(FRawLog.Count));
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
