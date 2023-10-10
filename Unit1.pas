unit Unit1;

interface

uses
        Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
        Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Math,
        Winapi.OpenGL, Winapi.OpenGLext, Vcl.ExtCtrls, obj, Camera,
        _meshify_simplify_quadric;

Type
        TVector3f = record
                X: Single;
                Y: Single;
                Z: Single;
        end;

        TVector4f = record
                R: Single;
                G: Single;
                B: Single;
                A: Single;
        end;

const
        VertexShaderCode: string = '#version 330 core' + sLineBreak +
          'layout (location = 0) in vec3 aPos;' + sLineBreak + 'void main()' +
          sLineBreak + '{' + sLineBreak +
          '    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);' +
          sLineBreak + '}';

        fragmentShaderSource: string = '#version 330 core\n' +
          'out vec4 FragColor;\n' + 'void main()\n' + '{\n' +
          '   FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n' + '}\n\0';

var
        cameraPos: TVector3f = (X: 0.0; Y: 0.0; Z: 3.0);
        cameraFront: TVector3f = (X: 0.0; Y: 0.0; Z: - 1.0);
        cameraUp: TVector3f = (X: 0.0; Y: 1.0; Z: 0.0);
        FragColor: TVector4f = (R: 1.0; G: 0.5; B: 0.2; A: 1.0);
        Ax, Mx: Single;
        Ay, My: Single;

type
        TForm1 = class(TForm)

                Timer1: TTimer;
                procedure FormCreate(Sender: TObject);
                procedure FormPaint(Sender: TObject);
                procedure FormKeyPress(Sender: TObject; var Key: Char);

        private
                procedure setupPixelFormat(DC: HDC);
                procedure GLInit;

                procedure load3DObjModel;
                procedure RenderModel(const vertices: TVertices;
                  const faces: TFaces);
                procedure SetupLightSource;
                procedure ApplicationEvents1Idle(Sender: TObject;
                  var Done: Boolean);
                procedure Render;
                procedure ResetModelView;

        var
                fDC: HDC;

        public

        end;

var
        Form1: TForm1;
        VBO, VAO: GLuint;
        angle: Single;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
        RC: HGLRC;
begin
        fDC := GetDC(Handle); // Actually, you can use any windowed control here
        setupPixelFormat(fDC);
        RC := wglCreateContext(fDC); // makes OpenGL window out of DC
        wglMakeCurrent(fDC, RC); // makes OpenGL window active

        InitOpenGLext;
        GLInit;
        load3DObjModel;

        Application.OnIdle := ApplicationEvents1Idle;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
        Done := False;
        Render;
end;

procedure TForm1.Render;
begin

        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
        ResetModelView;
        glRotatef(Ax, 1, 0, 0);
        glRotatef(-Ay, 0, 1, 0);
        // load3DObjModel;
        SwapBuffers(fDC);
end;

procedure TForm1.ResetModelView;
begin
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity;
        gluLookAt(0, 20, 120, 0, 30, 0, 0, 1, 0);
        // Set position and orientation
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
        if Key = #27 then
                Close;

end;

procedure TForm1.GLInit;

begin
        SetupCamera(ClientWidth, ClientHeight);;
        SetupLightSource;

end;

procedure TForm1.setupPixelFormat(DC: HDC);
const
        pfd: TPIXELFORMATDESCRIPTOR = (nSize: sizeof(TPIXELFORMATDESCRIPTOR);
          // size
          nVersion: 1; // version
          dwFlags: PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER;
          // support double-buffering
          iPixelType: PFD_TYPE_RGBA; // color type
          cColorBits: 24; // preferred color depth
          cRedBits: 0; cRedShift: 0; // color bits (ignored)
          cGreenBits: 0; cGreenShift: 0; cBlueBits: 0; cBlueShift: 0;
          cAlphaBits: 0; cAlphaShift: 0; // no alpha buffer
          cAccumBits: 0; cAccumRedBits: 0; // no accumulation buffer,
          cAccumGreenBits: 0; // accum bits (ignored)
          cAccumBlueBits: 0; cAccumAlphaBits: 0; cDepthBits: 16; // depth buffer
          cStencilBits: 0; // no stencil buffer
          cAuxBuffers: 0; // no auxiliary buffers
          iLayerType: PFD_MAIN_PLANE; // main layer

          bReserved: 0; dwLayerMask: 0; dwVisibleMask: 0; dwDamageMask: 0;
          // no layer, visible, damage masks
        );
var
        pixelFormat: integer;
begin
        pixelFormat := ChoosePixelFormat(DC, @pfd);
        if (pixelFormat = 0) then
                exit;
        if (SetPixelFormat(DC, pixelFormat, @pfd) <> TRUE) then
                exit;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
        // Draw;
        // load3DObjModel();

end;

procedure TForm1.load3DObjModel();
var
        FileName: string;
        faces: TFaces;
        vertices: TVertices;
        i: integer;
        Count, k: integer;
begin
        // Specify the path to your OBJ file
        // "C:\Users\t.vanotterloo\Pictures\GewoonKunst.obj" \
        // C:\Users\t.vanotterloo\Pictures\Robot.obj
        FileName := 'C:\Users\t.vanotterloo\Pictures\GewoonKunst.obj';

        // Call the LoadObj procedure to load the OBJ file
        try
                LoadObj(FileName, faces, vertices);
                RenderModel(vertices, faces);
        except
                // Exception handling code
                on E: Exception do
                begin
                        ShowMessage
                          ('An error occurred trying to load in the Object7: ' +
                          E.Message);
                        // You can perform additional error handling here if needed
                end;
        end;
        SetLength(vertices, 0);
        SetLength(faces, 0);
end;

procedure TForm1.RenderModel(const vertices: TVertices; const faces: TFaces);
var
        VBO: GLuint;
        vertexShader: GLuint;
        success: integer;
        infoLog: PAnsiChar;
        outputString: string;
        fragmentShader: integer;
        shaderProgram: integer;
        VAO: integer;
begin
        // //https://learnopengl.com/Getting-started/Hello-Triangle

        glGenBuffers(1, @VBO);

        glBindBuffer(GL_ARRAY_BUFFER, VBO);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices,
          GL_STATIC_DRAW);

        vertexShader := glCreateShader(GL_VERTEX_SHADER);

        glShaderSource(vertexShader, 1, @VertexShaderCode, nil);
        glCompileShader(vertexShader);

        glGetShaderiv(vertexShader, GL_COMPILE_STATUS, @success);
        OutputDebugStringA('test'); // logging for debugging
        if success = 0 then
        begin
                glGetShaderInfoLog(vertexShader, 512, nil, infoLog);
                // this line crashes.
                OutputDebugStringA(infoLog); // logging for debugging
        end;

        // fragment shader

        fragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
        glShaderSource(fragmentShader, 1, @fragmentShaderSource, nil);
        glCompileShader(fragmentShader);

        // shader program

        shaderProgram := glCreateProgram();

        glAttachShader(shaderProgram, vertexShader);
        glAttachShader(shaderProgram, fragmentShader);
        glLinkProgram(shaderProgram);

        glGetProgramiv(shaderProgram, GL_LINK_STATUS, @success);

        if success = 0 then
        begin
                //glGetProgramInfoLog(shaderProgram, 512, nil, infoLog);  Crash
                // print to console..
        end;

        glUseProgram(shaderProgram);

        glDeleteShader(vertexShader);
        glDeleteShader(fragmentShader);

        // Linking Vertex Attributes

        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(real),
          Pointer(0));

        glEnableVertexAttribArray(0);

        glGenVertexArrays(1, @VAO);

        // ..:: Initialization code (done once (unless your object frequently changes)) :: ..
        // 1. bind Vertex Array Object
        glBindVertexArray(VAO);
        // 2. copy our vertices array in a buffer for OpenGL to use
        glBindBuffer(GL_ARRAY_BUFFER, VBO);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices,
          GL_STATIC_DRAW);
        // 3. then set our vertex attributes pointers
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(real),
          Pointer(0));
        glEnableVertexAttribArray(0);

        // ..:: Drawing code (in render loop) :: ..
        // 4. draw the object
        while not glfwWindowShouldClose(window) do // dont have glfw
        begin
                glClearColor(0.2, 0.3, 0.3, 1.0);
                glClear(GL_COLOR_BUFFER_BIT);

                glUseProgram(shaderProgram);
                glBindVertexArray(VAO);
                glDrawArrays(GL_TRIANGLES, 0, sizeof(vertices));
                glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

                glfwSwapBuffers(window);
                glfwPollEvents();
        end;

end;

procedure TForm1.SetupLightSource;
begin
        // Set background color
        glClearColor(1.0, 0.569, 0.557, 1.0);
        // Enable lighting
        glEnable(GL_LIGHTING);
        // Configure and enable light source
        glEnable(GL_LIGHT0);
        // Enable lighting calculations for front and back faces
        glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
end;

end.
