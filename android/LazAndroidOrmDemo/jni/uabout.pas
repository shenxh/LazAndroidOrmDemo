{
  // about app form
  // A Lazarus Android ORM Demo
  // https://github.com/shenxh/LazAndroidOrmDemo
  // Shen Xue Hua , 1339838080@qq.com
}
unit uabout;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls;
  
type

  { TfrmAbout }

  TfrmAbout = class(jForm)
    jImageView1: jImageView;
    jImageView2: jImageView;
    jImageView3: jImageView;
    jPanel1: jPanel;
    jTextView1: jTextView;
    jTextView2: jTextView;
    jTextView3: jTextView;
    jTextView4: jTextView;
    jTextView5: jTextView;
    procedure jImageView2Click(Sender: TObject);
  private
    {private declarations}
  public
    {public declarations}
  end;

var
  frmAbout: TfrmAbout;

implementation
  
{$R *.lfm}
  

{ TfrmAbout }

procedure TfrmAbout.jImageView2Click(Sender: TObject);
begin
  Self.Close;
end;

end.
