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
    { private �錾 }
    FRawLog : TStringList;
    //
    procedure LoadLogFile(filepath : string);
    function GetFilteredLogText(filter: string): string;
    function ExtractSiteID(filter: string): string;
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
begin
  //
  // �v���b�g�t�H�[���ɂăN���b�v�{�[�h�����p�ł���̂��`�F�b�N����
  if TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Clipboard) then
  begin
    //
    // �N���b�v�{�[�h�ւ̃R�s�[�Ώۂ�I������
    if cbSiteId.ItemIndex>=0 then
    begin
      // �t�B���^�[���I������Ă���ꍇ�A���O���t�B���^�[�ɂĒ��o����
      CopyText := GetFilteredLogText(cbSiteId.Items[cbSiteId.ItemIndex]);
    end
    else
    begin
      // �S�Ẵ��O�����R�s�[�Ώۂɂ���
      CopyText := FRawLog.Text;
    end;
    //
    // �o�͑Ώۂ̃��O�f�[�^����荞�ݎ��Ɠ�����Ԃɖ߂����߉��s��}������
    CopyText := StringReplace(CopyText, '=LF;', #13#10, [rfReplaceAll]);
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
    if not Trim(FList.Text).IsEmpty then
    begin
      FList.SaveToFile('filtered.txt', TEncoding.Default);
    end;
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
const
  TMP_FILENAME : string = 'tmp.txt';
var
  FReplacer : string;
  FStr : string;
  FList : TStringList;
  ii : Integer;
  jj : Integer;
  FFilter : string;
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
    //FList.Add(FStr);
    //FList.SaveToFile('nolf.txt');
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
    // ���O��񂩂�s�v�ȏ��f�[�^���폜����
    if FList.Count>0 then
    begin
      for ii := (FList.Count - 1) downto 0 do
      begin
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
          FFilter := ExtractSiteID(FFilter);
          //
          for jj := 0 to cbSiteID.Items.Count - 1 do
          begin
            if String.Compare(cbSiteID.Items[jj], FFilter) = 0 then
            begin
              break;
            end;
          end;
          if (cbSiteID.Items.Count - 1) <> jj then
          begin
            cbSiteID.Items.Add(FFilter);
          end;
        end;
      end;
    end;
    //FList.SaveToFile('newlf2.txt');
    //
    // ���O�t�@�C�����}�[�W����
    FRawLog.AddStrings(FList);
    FRawLog.Sort;
    FRawLog.SaveToFile('sorted.txt', TEncoding.Default);
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
