{
  // login form
  // A Lazarus Android ORM Demo
  // https://github.com/shenxh/LazAndroidOrmDemo
  // Shen Xue Hua , 1339838080@qq.com
}
unit ulogin;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, preferences;
  
type

  { TfrmLogin }

  TfrmLogin = class(jForm)
    jButton1: jButton;
    jCheckBox1: jCheckBox;
    jTxtUser: jEditText;
    jTxtPass: jEditText;
    jImageView1: jImageView;
    jPanel1: jPanel;
    jPanel2: jPanel;
    jPanel3: jPanel;
    jPanel4: jPanel;
    jPreferences1: jPreferences;
    jTextView1: jTextView;
    jTextView2: jTextView;
    jTextView3: jTextView;
    procedure frmLoginCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure frmLoginJNIPrompt(Sender: TObject);
    procedure jButton1Click(Sender: TObject);
  private
    {private declarations}
  public
    {public declarations}
  end;

var
  frmLogin: TfrmLogin;

implementation

uses umain;
  
{$R *.lfm}
  

{ TfrmLogin }

procedure TfrmLogin.frmLoginJNIPrompt(Sender: TObject);
begin
  //Init
  jTxtUser.Clear;
  jTxtPass.Clear;
  jTxtUser.Text:=jPreferences1.GetStringData('lazandroidormdemo_user','');
  jTxtPass.Text:=jPreferences1.GetStringData('lazandroidormdemo_pass','');
  jTxtUser.SetFocus;

end;

procedure TfrmLogin.frmLoginCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //Application close
  gApp.Finish;
end;

procedure TfrmLogin.jButton1Click(Sender: TObject);
begin
  //Login
  if (trim(jTxtUser.Text)='') or (trim(jTxtPass.Text)='') then
  begin
    ShowMessage('User or password is empty!');
    Exit;
  end;
  //save Preferences
  if jCheckBox1.Checked then
    begin
      jPreferences1.SetStringData('lazandroidormdemo_user',jTxtUser.Text);
      jPreferences1.SetStringData('lazandroidormdemo_pass',jTxtPass.Text);
    end
  else
    begin
      jPreferences1.SetStringData('lazandroidormdemo_user','');
      jPreferences1.SetStringData('lazandroidormdemo_pass','');
    end;
  //enter main form
  if frmMain = nil then
  begin
    gApp.CreateForm(TfrmMain, frmMain);
    frmMain.Init(gApp);
  end
  else
  begin
    frmMain.Show;
  end;

end;

end.
