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
    Label2: TLabel;
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
    function ExtractSiteID(filter: string): string;
    function IsSfccLogFile(filepath: string): boolean;
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnClearClick(Sender: TObject);
begin
  //
  // ログ情報を全て消去する
  FRawLog.Clear;
  //
  // ログを絞り込むフィルター用サイトID情報を全て消去する
  cbSiteId.Items.Clear;
  //
  // ログ情報がないので、コピー操作を不可にする
  btnCopy.Enabled := false;
  //
  Memo1.Lines.Add('総登録行数:'+IntToStr(FRawLog.Count));
end;

procedure TForm1.btnCopyClick(Sender: TObject);
var
  Clipboard: IFMXClipboardService;
  CopyText: string;
begin
  //
  // プラットフォームにてクリップボードが利用できるのかチェックする
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Clipboard) then
  begin
    //
    // クリップボードへのコピー対象を選択する
    if cbSiteId.ItemIndex>=0 then
    begin
      // フィルターが選択されている場合、ログをフィルターにて抽出する
      CopyText := GetFilteredLogText(cbSiteId.Items[cbSiteId.ItemIndex]);
    end
    else
    begin
      // 全てのログ情報をコピー対象にする
      CopyText := FRawLog.Text;
    end;
    //
    // 出力対象のログデータを取り込み時と同じ状態に戻すため改行を挿入する
    CopyText := StringReplace(CopyText, '=LF;', #13#10, [rfReplaceAll]);
    CopyText := StringReplace(CopyText, #13#10#13#10, #13#10, [rfReplaceAll]);
    //
    // 対象文字列をクリップボードへコピーする
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
  FStr : String;
begin
  if (cbSiteId.Items.Count>0) then
  begin
    Memo1.Lines.Add(cbSiteId.Items[cbSiteId.ItemIndex]);
    FList := TStringList.Create;
    FStr := GetFilteredLogText(cbSiteId.Items[cbSiteId.ItemIndex]);
    FStr := StringReplace(FStr, '=LF;', #13#10, [rfReplaceAll]);
    FList.Add(FStr);
    //
    {
    if not Trim(FList.Text).IsEmpty then
    begin
      FList.SaveToFile('filtered.txt', TEncoding.Default);
    end;
    }
    FList.Clear;
    FList.Free;
  end;
end;

function TForm1.ExtractSiteID(filter: string): string;
var
  sidx: integer;
  eidx: integer;
begin
  //
  // デフォルト戻り値を設定する。サイトIDが無い場合は''を返す。
  result := '';
  //
  sidx := filter.IndexOf('Sites-');
  if sidx>0 then
  begin
    eidx := filter.IndexOf('-Site');
    if eidx>0 then
    begin
      //
      // パイプライン実行ログからサイトIDを取得する
      result := filter.Substring(sidx+Length('Sites-'), eidx-sidx-Length('Sites-'));
    end
  end
  else
  begin
    sidx := filter.IndexOf('/servlet/s/');
    if sidx>0 then
    begin
      eidx := filter.IndexOf('/dw/shop/v');
      if eidx>0 then
      begin
        //
        // OCAPI Shop-API実行ログからサイトIDを取得する
        result := filter.Substring(sidx+Length('/servlet/s/'), eidx-sidx-Length('/servlet/s/'));
      end
      else
      begin
        eidx := filter.IndexOf('/dw/data/v');
        if eidx>0 then
        begin
          //
          // OCAPI Data-API実行ログからサイトIDを取得する
          result := filter.Substring(sidx+Length('/servlet/s/'), eidx-sidx-Length('/servlet/s/'));
        end
      end;
    end
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
  FSidx: integer;
begin
  FList := TStringList.Create;
  try
    FList.BeginUpdate;
    for ii := 0 to FRawLog.Count - 1 do
    begin
      if (FRawLog[ii].IndexOf('Sites-'+filter+'-Site')>= 0) or
         (FRawLog[ii].IndexOf('/s/'+filter+'/dw/')>= 0)  then
      begin
        FSidx := FRawLog[ii].IndexOf(#8)+1;
        FList.Add(FRawLog[ii].Substring(FSidx));
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

function TForm1.IsSfccLogFile(filepath: string): boolean;
var
  filename: string;
begin
  //
  // デフォルトの戻り値を設定する
  result := false;
  //
  // ファイルパスからファイル名の部分だけ抽出する
  filename := System.IOUtils.TPath.GetFileName(filepath);
  if not filename.IsEmpty then
  begin
    //
    // 受け入れるログファイルの名前をチェックする
    if (filename.IndexOf('customdebug-')=0) or
       (filename.IndexOf('customerror-')=0) or
       (filename.IndexOf('custominfo-')=0) or
       (filename.IndexOf('custom-')=0) or
       (filename.IndexOf('service-')=0) then
    begin
      result := true;
    end;
  end;
end;

procedure TForm1.LoadLogFile(filepath: string);
const
  TMP_FILENAME : string = 'tmp.txt';
var
  FReplacer : string;
  FStr : string;
  FList : TStringList;
  ii : Integer;
  jj : Integer;
  FFilter : string;
  FLastFilter: string;
  FHasIt: boolean;
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
    FList.SaveToFile(TMP_FILENAME);
    //
    // 不要なログ情報を削除する
    FList.Clear;
    //
    // プラットフォームのテキストファイル形式にて読み直す
    // これにより、正しく改行制御を行う
    FList.LoadFromFile(TMP_FILENAME, TEncoding.Default);
    //
    // 作業ファイルを削除する
    TFile.Delete(TMP_FILENAME);
    //
    // ログファイルをスキャンして、さまざまな処理を行う
    if FList.Count>0 then
    begin
      FList.BeginUpdate;
      for ii := (FList.Count - 1) downto 0 do
      begin
        //
        // ログ情報から不要な情報データを削除する
        if (FList[ii].IndexOf('RepeatedMessageSuppressingFilter')>0) then
        begin
          FList.Delete(ii)
        end
        else
        begin
          //
          // ログ情報の先頭部分を切り出し、サイトID情報を抽出する
          // 抽出したサイトID情報は、
          // フィルター用のサイトID コンボボックスの選択肢情報として
          // 追加する。ただし、サイトIDは重複しないようにする
          FFilter := FList[ii].Substring(60,40);
          if String.Compare(FFilter, FLastFilter) <> 0 then
          begin
            //
            // フィルター文字列を次回比較のため設定する
            FLastFilter := FFilter;
            //
            // フィルター文字列からサイトIDを抽出し登録する
            FFilter := ExtractSiteID(FFilter);
            //
            // サイトIDが登録済みかチェックする
            FHasIt := false;
            for jj := 0 to cbSiteID.Items.Count - 1 do
            begin
              if String.Compare(cbSiteID.Items[jj], FFilter) = 0 then
              begin
                FHasIt := true;
                break;
              end;
            end;
            if (not FHasIt) and (not Trim(FFilter).IsEmpty) then
            begin
              cbSiteID.Items.Add(FFilter);
            end;
          end;
          //
          //
          FFilter := FList[ii].Substring(0,29)+IntToStr(ii)+#8;
          FList[ii] := FFilter + FList[ii];
        end;
      end;
      FList.EndUpdate;
    end;
    //FList.SaveToFile('newlf2.txt');
    //
    // ログファイルをマージする
    FRawLog.AddStrings(FList);
    FRawLog.Sort;
    {
    //
    // デバッグ中にファイルロックで停止するのを防止するため
    try
      FRawLog.SaveToFile('sorted.txt', TEncoding.Default);
    finally
      //
      Memo1.Lines.Add('sorted.txt is locked. It may be opened by editor.');
    end;
    }
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
    //
    // 時間を要する処理のためカーソルを待ち状態に変更する
    RoundRect1.Cursor := crHourGlass;
    //Memo1.Lines.Clear;
    Memo1.BeginUpdate;
    //
    // ログファイルがドロップされたので、クリップボード曽佐を有効にする
    btnCopy.Enabled := true;
    //
    // ドロップされたファイル情報を基に、各ファイルをログとして読み込む
    // 読込時に、随時ファイル結合と並べ替えを行う。
    for ii := 0 to (Length(Data.Files) - 1) do
    begin
      //
      // 処理対象ログファイルをファイル名にてチェックする
      if IsSfccLogFile(Data.Files[ii]) then
      begin
        Memo1.Lines.Add(Data.Files[ii]);
        //
        LoadLogFile(Data.Files[ii]);
      end;
    end;
    Memo1.EndUpdate;
    //
    // 処理が完了したので、カーソルを基に戻す
    RoundRect1.Cursor := crDefault;
  end;
end;

procedure TForm1.Panel2DragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
    Operation := TDragOperation.Copy;
end;

end.
