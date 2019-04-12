{
  // my settings form
  // A Lazarus Android ORM Demo
  // https://github.com/shenxh/LazAndroidOrmDemo
  // Shen Xue Hua , 1339838080@qq.com
}
unit umysettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls;
  
type

  { TfrmSettings }

  TfrmSettings = class(jForm)
    jImageView2: jImageView;
    jPanel1: jPanel;
    jTextView1: jTextView;
    procedure jImageView2Click(Sender: TObject);
  private
    {private declarations}
  public
    {public declarations}
  end;

var
  frmSettings: TfrmSettings;

implementation
  
{$R *.lfm}
  

{ TfrmSettings }

procedure TfrmSettings.jImageView2Click(Sender: TObject);
begin
  //close
  Self.Close;
end;

end.
