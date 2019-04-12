{
  // Input Info form
  // A Lazarus Android ORM Demo
  // https://github.com/shenxh/LazAndroidOrmDemo
  // Shen Xue Hua , 1339838080@qq.com
}
unit uinputinfo;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls;
  
type

  { TfrmInputInfo }

  TfrmInputInfo = class(jForm)
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
  frmInputInfo: TfrmInputInfo;

implementation
  
{$R *.lfm}
  

{ TfrmInputInfo }

procedure TfrmInputInfo.jImageView2Click(Sender: TObject);
begin
  //Close
  Self.Close;
end;

end.
