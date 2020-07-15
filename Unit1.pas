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
    cbCommentHeader: TCheckBox;
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
    function GetFilteredLog(filter: string): TStringList;
    // function GetFilteredLogText(filter: string): string;
    function ExtractSiteID(filter: string): string;
    function ExtractTimestamp(filter: string): string;
    function ExtractEmail(filter: string): string;
    function ExtractLine(list: TStringList; target:string): string;
    function ExtractVersion(filter: string): string;
    function ExtractWord(filter: string; startword:string; endword:string): string;
    function IsSfccLogFile(filepath: string): boolean;
    function GetLogHeader(list:TStringList; siteid: string): string;
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
  FList: TStringList;
  FSiteid: string;
  //
  // コメントヘッダ用
  FCopyHeader: string;
begin
  //
  // ローカル変数の初期化
  FList := nil;
  FSiteid := '';
  FCopyHeader := '';
  //** ---------------------------------------------------------------------
  //* ログデータベースから指定SiteIDだけのログを抽出する
  //--------------------------------------------------------------------- */
  //
  // プラットフォームにてクリップボードが利用できるのかチェックする
  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Clipboard) then
  begin
    //
    // 利用できない場合、処理を行わず終了する
    exit;
  end;
  //
  // クリップボードへのコピー対象を選択する
  if cbSiteId.ItemIndex>=0 then
  begin
    FSiteid := cbSiteId.Items[cbSiteId.ItemIndex];
    // フィルターが選択されている場合、ログをフィルターにて抽出する
    FList := GetFilteredLog(FSiteid);
    CopyText := FList.Text;
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
  //** ---------------------------------------------------------------------
  //* ログのコメントヘッダを作成する
  //--------------------------------------------------------------------- */
  if cbCommentHeader.IsChecked and Assigned(FList) and (FList.Count>0) then
  begin
    //
    //
    FCopyHeader := GetLogHeader(FList, FSiteid);
    FList.Clear;
    FList.Free;
    //
    // 対象文字列をクリップボードへコピーする
    Clipboard.SetClipboard(FCopyHeader + CopyText);
  end
  else
  begin
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
begin
  if (cbSiteId.Items.Count>0) then
  begin
    Memo1.Lines.Add(cbSiteId.Items[cbSiteId.ItemIndex]);
  end;
end;

function TForm1.ExtractEmail(filter: string): string;
resourcestring
  START_WORD1 = '"param":[{"emailAddress":"';
  END_WORD1   = '"}]}';
begin
  result := ExtractWord(filter, START_WORD1, END_WORD1);
end;

function TForm1.ExtractLine(list: TStringList; target: string): string;
var
  ii:integer;
begin
  result := '';
  //
  if Assigned(list) and (list.Count>0) then
  begin
    for ii := 0 to list.Count - 1 do
    begin
      result := list[ii];
      if result<>'' then
      begin
        if result.IndexOf(target)>=0 then
        begin
          break;
        end;
      end;
    end;
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

function TForm1.ExtractTimestamp(filter: string): string;
begin
  result := filter.Substring(1, 27);
end;

function TForm1.ExtractVersion(filter: string): string;
const
  START_WORD1 = '/dw/shop/';
  END_WORD1   = '/customers';
  START_WORD2 = '/dw/shop/';
  END_WORD2   = '/baskets';
//var
//  sidx: integer;
//  eidx: integer;
begin
  result := ExtractWord(filter, START_WORD1, END_WORD1);
{
  //
  // デフォルト戻り値を設定する。サイトIDが無い場合は''を返す。
  result := '';
  //
  sidx := filter.IndexOf(START_WORD1);
  if sidx>0 then
  begin
    eidx := filter.IndexOf(END_WORD1);
    if eidx>0 then
    begin
      //
      // パイプライン実行ログからサイトIDを取得する
      result := filter.Substring(sidx+Length(START_WORD1), eidx-sidx-Length(END_WORD1));
    end
    else
    begin
      eidx := filter.IndexOf(END_WORD2);
      if eidx>0 then
      begin
        //
        // パイプライン実行ログからサイトIDを取得する
        result := filter.Substring(sidx+Length(START_WORD2), eidx-sidx-Length(END_WORD2));
      end
    end;
  end;
}
end;

function TForm1.ExtractWord(filter, startword, endword: string): string;
var
  sidx: integer;
  eidx: integer;
begin
  //
  // デフォルト戻り値を設定する。サイトIDが無い場合は''を返す。
  result := '';
  //
  sidx := filter.IndexOf(startword);
  if sidx>=0 then
  begin
    eidx := filter.IndexOf(endword);
    if eidx>sidx then
    begin
      //
      // パイプライン実行ログからサイトIDを取得する
      //result := filter.Substring(sidx+Length(startword), eidx-sidx-Length(endword));
      result := filter.Substring(sidx+Length(startword), eidx-sidx-Length(startword));
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

function TForm1.GetFilteredLog(filter: string): TStringList;
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
  Result := FList;
  //FList.Clear;
  //FList.Free;
end;

function TForm1.GetLogHeader(list:TStringList; siteid: string): string;

  function separater: string;
  var
    ii: integer;
  begin
    result := '';
    //
    for ii := 0 to 79 do
    begin
      result := result + '=';
    end;
  end;

resourcestring
  template = '検査ID  　: 記載してください'#13#10
           + '検査概要　: 記載してください'#13#10
           + '検査環境　: @instance'#13#10
           + '検査サイト: @instance @siteid'#13#10
           + 'OCAPI Ver.: @version'#13#10
           + '検査装置　: 検査用サーバ'#13#10
           + 'ログ対象　: 検査用サーバ側にて記録しました。'#13#10
           + '実施者　　: @email'#13#10
           + '実施日時　: @timestamp'#13#10;
var
  header : string;
  //
  FLine: string;
  FInstance:string;
  FVersion:string;
  FEmail:string;
  FTimestamp:string;
begin
  //
  // ログからヘッダ情報を取得する
  FInstance := 'Staging / Sandbox';
  FVersion := '';
  FEmail := '';
  FTimestamp := '';
  //
  // 抽出したログからヘッダ情報を抽出する
  if Assigned(list) and (list.Count>0) then
  begin
    //
    FLine := ExtractLine(list, 'ShopAPIServlet');
    FTimestamp := ExtractTimestamp(FLine);
    FVersion := ExtractVersion(FLine);
    //
    FLine := ExtractLine(list, '"param":[{"emailAddress":"');
    FEmail := ExtractEmail(FLine);
  end;
  //
  // キーワードでテンプレートを編集する
  header := StringReplace(template, '@instance', FInstance, [rfReplaceAll]);
  //
  header := StringReplace(header, '@siteid', siteid, [rfReplaceAll]);
  //
  header := StringReplace(header, '@version', FVersion, [rfReplaceAll]);
  //
  header := StringReplace(header, '@email', FEmail, [rfReplaceAll]);
  //
  header := StringReplace(header, '@timestamp', FTimestamp, [rfReplaceAll]);
  //
  result := separater + #13#10 + header + separater + #13#10 ;
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
resourcestring
  TMP_FILENAME = 'tmp.txt';
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
