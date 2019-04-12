{
  // other form
  // A Lazarus Android ORM Demo
  // https://github.com/shenxh/LazAndroidOrmDemo
  // Shen Xue Hua , 1339838080@qq.com
}
unit uother;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls;
  
type

  { TfrmOther }

  TfrmOther = class(jForm)
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
  frmOther: TfrmOther;

implementation
  
{$R *.lfm}
  

{ TfrmOther }

procedure TfrmOther.jImageView2Click(Sender: TObject);
begin
  //close
  Self.Close;
end;

end.
