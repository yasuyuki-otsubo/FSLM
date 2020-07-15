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
    { private �錾 }
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
    { public �錾 }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnClearClick(Sender: TObject);
begin
  //
  // ���O����S�ď�������
  FRawLog.Clear;
  //
  // ���O���i�荞�ރt�B���^�[�p�T�C�gID����S�ď�������
  cbSiteId.Items.Clear;
  //
  // ���O��񂪂Ȃ��̂ŁA�R�s�[�����s�ɂ���
  btnCopy.Enabled := false;
  //
  Memo1.Lines.Add('���o�^�s��:'+IntToStr(FRawLog.Count));
end;

procedure TForm1.btnCopyClick(Sender: TObject);
var
  Clipboard: IFMXClipboardService;
  CopyText: string;
  FList: TStringList;
  FSiteid: string;
  //
  // �R�����g�w�b�_�p
  FCopyHeader: string;
begin
  //
  // ���[�J���ϐ��̏�����
  FList := nil;
  FSiteid := '';
  FCopyHeader := '';
  //** ---------------------------------------------------------------------
  //* ���O�f�[�^�x�[�X����w��SiteID�����̃��O�𒊏o����
  //--------------------------------------------------------------------- */
  //
  // �v���b�g�t�H�[���ɂăN���b�v�{�[�h�����p�ł���̂��`�F�b�N����
  if not TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Clipboard) then
  begin
    //
    // ���p�ł��Ȃ��ꍇ�A�������s�킸�I������
    exit;
  end;
  //
  // �N���b�v�{�[�h�ւ̃R�s�[�Ώۂ�I������
  if cbSiteId.ItemIndex>=0 then
  begin
    FSiteid := cbSiteId.Items[cbSiteId.ItemIndex];
    // �t�B���^�[���I������Ă���ꍇ�A���O���t�B���^�[�ɂĒ��o����
    FList := GetFilteredLog(FSiteid);
    CopyText := FList.Text;
  end
  else
  begin
    // �S�Ẵ��O�����R�s�[�Ώۂɂ���
    CopyText := FRawLog.Text;
  end;
  //
  // �o�͑Ώۂ̃��O�f�[�^����荞�ݎ��Ɠ�����Ԃɖ߂����߉��s��}������
  CopyText := StringReplace(CopyText, '=LF;', #13#10, [rfReplaceAll]);
  CopyText := StringReplace(CopyText, #13#10#13#10, #13#10, [rfReplaceAll]);
  //
  //** ---------------------------------------------------------------------
  //* ���O�̃R�����g�w�b�_���쐬����
  //--------------------------------------------------------------------- */
  if cbCommentHeader.IsChecked and Assigned(FList) and (FList.Count>0) then
  begin
    //
    //
    FCopyHeader := GetLogHeader(FList, FSiteid);
    FList.Clear;
    FList.Free;
    //
    // �Ώە�������N���b�v�{�[�h�փR�s�[����
    Clipboard.SetClipboard(FCopyHeader + CopyText);
  end
  else
  begin
    //
    // �Ώە�������N���b�v�{�[�h�փR�s�[����
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
  // �f�t�H���g�߂�l��ݒ肷��B�T�C�gID�������ꍇ��''��Ԃ��B
  result := '';
  //
  sidx := filter.IndexOf('Sites-');
  if sidx>0 then
  begin
    eidx := filter.IndexOf('-Site');
    if eidx>0 then
    begin
      //
      // �p�C�v���C�����s���O����T�C�gID���擾����
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
        // OCAPI Shop-API���s���O����T�C�gID���擾����
        result := filter.Substring(sidx+Length('/servlet/s/'), eidx-sidx-Length('/servlet/s/'));
      end
      else
      begin
        eidx := filter.IndexOf('/dw/data/v');
        if eidx>0 then
        begin
          //
          // OCAPI Data-API���s���O����T�C�gID���擾����
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
  // �f�t�H���g�߂�l��ݒ肷��B�T�C�gID�������ꍇ��''��Ԃ��B
  result := '';
  //
  sidx := filter.IndexOf(START_WORD1);
  if sidx>0 then
  begin
    eidx := filter.IndexOf(END_WORD1);
    if eidx>0 then
    begin
      //
      // �p�C�v���C�����s���O����T�C�gID���擾����
      result := filter.Substring(sidx+Length(START_WORD1), eidx-sidx-Length(END_WORD1));
    end
    else
    begin
      eidx := filter.IndexOf(END_WORD2);
      if eidx>0 then
      begin
        //
        // �p�C�v���C�����s���O����T�C�gID���擾����
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
  // �f�t�H���g�߂�l��ݒ肷��B�T�C�gID�������ꍇ��''��Ԃ��B
  result := '';
  //
  sidx := filter.IndexOf(startword);
  if sidx>=0 then
  begin
    eidx := filter.IndexOf(endword);
    if eidx>sidx then
    begin
      //
      // �p�C�v���C�����s���O����T�C�gID���擾����
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
  Memo1.Lines.Add('�o�^�s��:'+IntToStr(FRawLog.Count))
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
  template = '����ID  �@: �L�ڂ��Ă�������'#13#10
           + '�����T�v�@: �L�ڂ��Ă�������'#13#10
           + '�������@: @instance'#13#10
           + '�����T�C�g: @instance @siteid'#13#10
           + 'OCAPI Ver.: @version'#13#10
           + '�������u�@: �����p�T�[�o'#13#10
           + '���O�Ώہ@: �����p�T�[�o���ɂċL�^���܂����B'#13#10
           + '���{�ҁ@�@: @email'#13#10
           + '���{�����@: @timestamp'#13#10;
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
  // ���O����w�b�_�����擾����
  FInstance := 'Staging / Sandbox';
  FVersion := '';
  FEmail := '';
  FTimestamp := '';
  //
  // ���o�������O����w�b�_���𒊏o����
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
  // �L�[���[�h�Ńe���v���[�g��ҏW����
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
  // �f�t�H���g�̖߂�l��ݒ肷��
  result := false;
  //
  // �t�@�C���p�X����t�@�C�����̕����������o����
  filename := System.IOUtils.TPath.GetFileName(filepath);
  if not filename.IsEmpty then
  begin
    //
    // �󂯓���郍�O�t�@�C���̖��O���`�F�b�N����
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
    // �h���b�O���ꂽ�t�@�C����S�ēǂ�
    FReplacer := TFile.ReadAllText(filepath, TEncoding.UTF8);
    // -----------------------------------------
    //  ���O�����P���A�P�s�ɕԊ҂���
    // -----------------------------------------
    //
    // ��U LF���s����菜��
    FStr := StringReplace(FReplacer, #10, '=LF;', [rfReplaceAll]);
    FList := TStringList.Create;
    //
    // �S�ă^�C���X�^���v���擪�ɂȂ�悤�ɉ��s��}������
    //FReplacer := StringReplace(FStr, '=LF;[', #10'[', [rfReplaceAll]); // LF
    FReplacer := StringReplace(FStr, '=LF;[', #13#10'[', [rfReplaceAll]);
    FList.Clear;
    FList.Add(FReplacer);
    FList.SaveToFile(TMP_FILENAME);
    //
    // �s�v�ȃ��O�����폜����
    FList.Clear;
    //
    // �v���b�g�t�H�[���̃e�L�X�g�t�@�C���`���ɂēǂݒ���
    // ����ɂ��A���������s������s��
    FList.LoadFromFile(TMP_FILENAME, TEncoding.Default);
    //
    // ��ƃt�@�C�����폜����
    TFile.Delete(TMP_FILENAME);
    //
    // ���O�t�@�C�����X�L�������āA���܂��܂ȏ������s��
    if FList.Count>0 then
    begin
      FList.BeginUpdate;
      for ii := (FList.Count - 1) downto 0 do
      begin
        //
        // ���O��񂩂�s�v�ȏ��f�[�^���폜����
        if (FList[ii].IndexOf('RepeatedMessageSuppressingFilter')>0) then
        begin
          FList.Delete(ii)
        end
        else
        begin
          //
          // ���O���̐擪������؂�o���A�T�C�gID���𒊏o����
          // ���o�����T�C�gID���́A
          // �t�B���^�[�p�̃T�C�gID �R���{�{�b�N�X�̑I�������Ƃ���
          // �ǉ�����B�������A�T�C�gID�͏d�����Ȃ��悤�ɂ���
          FFilter := FList[ii].Substring(60,40);
          if String.Compare(FFilter, FLastFilter) <> 0 then
          begin
            //
            // �t�B���^�[������������r�̂��ߐݒ肷��
            FLastFilter := FFilter;
            //
            // �t�B���^�[�����񂩂�T�C�gID�𒊏o���o�^����
            FFilter := ExtractSiteID(FFilter);
            //
            // �T�C�gID���o�^�ς݂��`�F�b�N����
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
    // ���O�t�@�C�����}�[�W����
    FRawLog.AddStrings(FList);
    FRawLog.Sort;
    {
    //
    // �f�o�b�O���Ƀt�@�C�����b�N�Œ�~����̂�h�~���邽��
    try
      FRawLog.SaveToFile('sorted.txt', TEncoding.Default);
    finally
      //
      Memo1.Lines.Add('sorted.txt is locked. It may be opened by editor.');
    end;
    }
    //
    Memo1.Lines.Add('�o�^�s��:'+IntToStr(FList.Count));
    //Memo1.Lines.AddStrings(FList);
    Memo1.Lines.Add('���o�^�s��:'+IntToStr(FRawLog.Count));
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
    // ���Ԃ�v���鏈���̂��߃J�[�\����҂���ԂɕύX����
    RoundRect1.Cursor := crHourGlass;
    //Memo1.Lines.Clear;
    Memo1.BeginUpdate;
    //
    // ���O�t�@�C�����h���b�v���ꂽ�̂ŁA�N���b�v�{�[�h�]����L���ɂ���
    btnCopy.Enabled := true;
    //
    // �h���b�v���ꂽ�t�@�C��������ɁA�e�t�@�C�������O�Ƃ��ēǂݍ���
    // �Ǎ����ɁA�����t�@�C�������ƕ��בւ����s���B
    for ii := 0 to (Length(Data.Files) - 1) do
    begin
      //
      // �����Ώۃ��O�t�@�C�����t�@�C�����ɂă`�F�b�N����
      if IsSfccLogFile(Data.Files[ii]) then
      begin
        Memo1.Lines.Add(Data.Files[ii]);
        //
        LoadLogFile(Data.Files[ii]);
      end;
    end;
    Memo1.EndUpdate;
    //
    // ���������������̂ŁA�J�[�\������ɖ߂�
    RoundRect1.Cursor := crDefault;
  end;
end;

procedure TForm1.Panel2DragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
    Operation := TDragOperation.Copy;
end;

end.
