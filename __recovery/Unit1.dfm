object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'BASIC OpenGL Program'
  ClientHeight = 549
  ClientWidth = 745
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  TextHeight = 15
  object StaticText1: TStaticText
    Left = 448
    Top = 120
    Width = 61
    Height = 19
    Caption = 'StaticText1'
    TabOrder = 0
  end
  object GLAsyncTimer1: TGLAsyncTimer
    Enabled = True
    Interval = 100
    OnTimer = GLAsyncTimer1Timer
    Left = 368
    Top = 280
  end
end
