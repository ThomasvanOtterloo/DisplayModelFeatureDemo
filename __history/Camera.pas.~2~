unit Camera;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Math,
  Winapi.OpenGL, Winapi.OpenGLext, Vcl.ExtCtrls, obj, _meshify_simplify_quadric;
Procedure SetupCamera(ClientWidth: Integer; ClientHeight: Integer);
Procedure SetupMouseUsage(Ax: Single; Mx: Single; Ay: Single; My: Single);

implementation

Procedure SetupCamera(ClientWidth: Integer; ClientHeight: Integer);
type
  TVector3f = record
    X: Single;
    Y: Single;
    Z: Single;
  end;
var
  cameraPos, cameraFront, cameraUp: TVector3f;

begin
  // cameraPos.X := 0.0;
  // cameraPos.Y := 0.0;
  // cameraPos.Z := 3.0;
  //
  // cameraFront.X := 0.0;
  // cameraFront.Y := 0.0;
  // cameraFront.Z := -1.0;
  //
  // cameraUp.X := 0.0;
  // cameraUp.Y := 5.0;
  // cameraUp.Z := 0.0;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(1, ClientWidth / ClientHeight, 3, 30.0);
  glRotatef(15.0, 1.0, 0.0, 0.0);


  // gluLookAt(
  // cameraPos.X, cameraPos.Y,cameraPos.Z,
  // cameraFront.X, cameraFront.Y,cameraFront.Z,
  // cameraUp.X, cameraUp.Y, cameraUp.Z
  // );

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glTranslatef(0.0, 3.0, 0.0);

  // https://learnopengl.com/book/book_pdf.pdf
  // maybe? Need to translate from C++ to Delphi tho..

  // Set the initial camera position (move back along the z-axis)

  glEnable(GL_DEPTH_TEST);
end;

Procedure SetupMouseUsage(Ax: Single; Mx: Single; Ay: Single; My: Single);
begin

  glRotatef(Ax, 1, 0, 0); // where in my code?
  glRotatef(-Ay, 0, 1, 0); // where in my code?

end;

end.
