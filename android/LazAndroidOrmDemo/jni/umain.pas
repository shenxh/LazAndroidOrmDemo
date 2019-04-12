{
  // main form
  // A Lazarus Android ORM Demo
  // https://github.com/shenxh/LazAndroidOrmDemo
  // Shen Xue Hua , 1339838080@qq.com
}
unit umain;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, imagefilemanager;
  
type

  { TfrmMain }

  TfrmMain = class(jForm)
    jDialogYN1: jDialogYN;
    jImageFileManager1: jImageFileManager;
    jImageHome: jImageView;
    jImageMine: jImageView;
    jImageView1: jImageView;
    jImageView2: jImageView;
    jListViewHome: jListView;
    jListViewMine: jListView;
    jPanel1: jPanel;
    jPanelMine: jPanel;
    jPanelHome: jPanel;
    jPanel2: jPanel;
    jPanelMainMenu: jPanel;
    jTextView1: jTextView;
    jTextView2: jTextView;
    procedure frmMainCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure frmMainJNIPrompt(Sender: TObject);
    procedure jDialogYN1ClickYN(Sender: TObject; YN: TClickYN);
    procedure jImageHomeClick(Sender: TObject);
    procedure jImageMineClick(Sender: TObject);
    procedure jListViewHomeClickItem(Sender: TObject; itemIndex: integer;
      itemCaption: string);
    procedure jListViewMineClickItem(Sender: TObject; itemIndex: integer;
      itemCaption: string);
  private
    {private declarations}
    iClose:Boolean;
  public
    {public declarations}
  end;

var
  frmMain: TfrmMain;

implementation

uses
  ucommon,uabout,umysettings,uinputinfo,upersonnel,uother,umyphoto;
  
{$R *.lfm}
  

{ TfrmMain }

procedure TfrmMain.frmMainJNIPrompt(Sender: TObject);
begin
  //Init
  if Not(gInitOk) then
  begin
    jImageHome.ImageIdentifier:='home2';
    jImageMine.ImageIdentifier:='mine1';
    jPanelHome.Height:=Self.Height - jPanelMainMenu.Height;
    jPanelHome.Visible:=true;
    jPanelMine.Height:=Self.Height - jPanelMainMenu.Height;
    jPanelMine.Visible:=false;
    //Home - Menu List
    jListViewHome.Clear;
    jListViewHome.Add('Input Info','|',colbrDefault,18,wgNone,'',jImageFileManager1.LoadFromAssets('edit1.png'));
    jListViewHome.Add('Personnel','|',colbrDefault,18,wgNone,'',jImageFileManager1.LoadFromAssets('personnel1.png'));
    jListViewHome.Add('Other','|',colbrDefault,18,wgNone,'',jImageFileManager1.LoadFromAssets('otherlist1.png'));

    //Mine - Menu List
    jListViewMine.Clear;
    jListViewMine.Add('My Photo','|',colbrDefault,18,wgNone,'',jImageFileManager1.LoadFromAssets('picture1.png'));
    jListViewMine.Add('My Settings','|',colbrDefault,18,wgNone,'',jImageFileManager1.LoadFromAssets('setting1.png'));
    jListViewMine.Add('About APP','|',colbrDefault,18,wgNone,'',jImageFileManager1.LoadFromAssets('about1.png'));
  end;
  gInitOk:=true;
end;

procedure TfrmMain.frmMainCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //Exit
  jDialogYN1.Show;
  if iClose then CanClose:=true else CanClose:=false;

end;

procedure TfrmMain.jDialogYN1ClickYN(Sender: TObject; YN: TClickYN);
begin
  //Exit App ?
  case YN of
    ClickYes:begin
      iClose:=true;
      gApp.Finish;
    end;
    ClickNo:begin
      iClose:=false;
      exit;
    end;
  end;

end;

procedure TfrmMain.jImageHomeClick(Sender: TObject);
begin
  //Home Click
  jImageHome.ImageIdentifier:='home2';
  jImageMine.ImageIdentifier:='mine1';
  jPanelMine.Visible:=false;
  jPanelHome.Visible:=true;

end;

procedure TfrmMain.jImageMineClick(Sender: TObject);
begin
  //Mine Click
  jImageHome.ImageIdentifier:='home1';
  jImageMine.ImageIdentifier:='mine2';
  jPanelMine.Visible:=true;
  jPanelHome.Visible:=false;

end;

procedure TfrmMain.jListViewHomeClickItem(Sender: TObject; itemIndex: integer;
  itemCaption: string);
begin
  //Click Home Menu
  //ShowMessage(itemCaption);
    case itemIndex of
      0:begin
        //Input Info
        if frmInputInfo = nil then
        begin
          gApp.CreateForm(TfrmInputInfo, frmInputInfo);
          frmInputInfo.Init(gApp);
        end
        else
        begin
          frmInputInfo.Show;
        end;
      end;
      1:begin
        //Personnel
        if frmPersonnel = nil then
        begin
          gApp.CreateForm(TfrmPersonnel, frmPersonnel);
          frmPersonnel.Init(gApp);
        end
        else
        begin
          frmPersonnel.Show;
        end;
      end;
      2:begin
        //Other
        if frmOther = nil then
        begin
          gApp.CreateForm(TfrmOther, frmOther);
          frmOther.Init(gApp);
        end
        else
        begin
          frmOther.Show;
        end;
      end;
    end;
end;

procedure TfrmMain.jListViewMineClickItem(Sender: TObject; itemIndex: integer;
  itemCaption: string);
begin
  //Click Mine Menu
  //ShowMessage(itemCaption);
  Case itemIndex of
    0:begin
       //My Photo
      if frmMyPhoto = nil then
      begin
        gApp.CreateForm(TfrmMyPhoto, frmMyPhoto);
        frmMyPhoto.Init(gApp);
      end
      else
      begin
        frmMyPhoto.Show;
      end;
    end;
    1:begin
       //My Settings
      if frmSettings = nil then
      begin
        gApp.CreateForm(TfrmSettings, frmSettings);
        frmSettings.Init(gApp);
      end
      else
      begin
        frmSettings.Show;
      end;
    end;
    2:begin
      //About APP
      if frmAbout = nil then
      begin
        gApp.CreateForm(TfrmAbout, frmAbout);
        frmAbout.Init(gApp);
      end
      else
      begin
        frmAbout.Show;
      end;

    end;
  end;
end;

end.
