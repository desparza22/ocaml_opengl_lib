Caml1999M028����            ;lib/util/shaders/shaders.ml����  P  �  )�  %I�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���-ppxlib_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������/ppx_optcomp.env�@�@@�������#env��&_none_A@ ��A@ �A��A@ ��A@ �A@@���-ocaml_version����'Defined��A@ ��A@ �A�������!4@��A@ ��A@ �A@@����"11@��#A@ ��$A@ �A@@����!2@��+A@ ��,A@ �A@@@��.A@ ��/A@ �A@@��1A@ ��2A@ �A@@@��4A@ ��5A@ �A@@@�@@�������@�@@@�@@�@@@@�@@@�@�ڠ��@������"()��;lib/util/shaders/shaders.mlA@@�A@@@@��A@@�A@@@@@���������-Ppx_bench_lib5Benchmark_accumulator/Current_libname#set��A@@�A@@@��A@@�A@@@@@��@���'shaders@�� A@@�!A@@@@@@��#A@@�$A@@@@@@��&A@@�'A@@@@��)A@@�*A@@@���@������"()��6A@@�7A@@@@��9A@@�:A@@@@@��������5Expect_test_collector,Current_file#set��GA@@�HA@@@��JA@@�KA@@@@@���1absolute_filename���R8@��TA@@�UA@@@@@@��WA@@�XA@@@@@@��ZA@@�[A@@@@��]A@@�^A@@@���@������4��iA@@�jA@@@@��lA@@�mA@@@@@��������3Ppx_inline_test_lib'Runtime5set_lib_and_partition��zA@@�{A@@@��}A@@�~A@@@@@��@���fi@���A@@��A@@@@@��@��� r@���A@@��A@@@@@@���A@@��A@@@@@@���A@@��A@@@@���A@@��A@@@��������$Tgl3���A@E��A@I@���A@E��A@I@@A���A@@��A@I@@���A@@��A@I@��������$Core���BJP��BJT@���BJP��BJT@@@���BJJ��BJT@@���BJJ��BJT@������$Util���DV]��DVa@�����@�����2rotation_functions���Ekq��Ek C@���Ekq��Ek C@@@���
  m       
     mat4 RotationX(float t) {
     mat4 ret = mat4(1.0);
     ret[1][1] = cos(t); ret[2][1] = -sin(t);
     ret[1][2] = sin(t); ret[2][2] = cos(t);
     return ret;
     }


     mat4 RotationY(float t) {
     mat4 ret = mat4(1.0);
     ret[0][0] =  cos(t); ret[2][0] = sin(t);
     ret[0][2] = -sin(t); ret[2][2] = cos(t);    
     return ret;
     }

     mat4 RotationZ(float t) {
     mat4 ret = mat4(1.0);
     ret[0][0] = cos(t); ret[1][0] = -sin(t);
     ret[0][1] = sin(t); ret[1][1] = cos(t);
     return ret;
     }

     mat4 Translation(vec3 xyz) {
     mat4 ret = mat4(1.0);
     ret[3][0] = xyz[0];
     ret[3][1] = xyz[1];
     ret[3][2] = xyz[2];
     return ret;
     }

     mat4 Scaling(vec3 xyz) {
     mat4 ret = mat4(0.);
     ret[0][0] = xyz[0];
     ret[1][1] = xyz[1];
     ret[2][2] = xyz[2];
     ret[3][3] = 1;
     return ret;
     }
     �@���F F J��m��@@@@���Ekm��m��@@���Ekm��m��@@���DVd��n��@@@���DVV��n��@���DVV��n��@������(Standard���p����p��@�����@�����4vertex_shader_color3��
s���s��@��s���s��@@@���	�#version 330 core
     in vec3 vertex;
     in vec3 color;
     out vec4 v_color;
     void main()
     {
     v_color = vec4(color, 1.0);
     gl_Position = vec4(vertex, 1.0);
     }�@��t���|��@@@@��s���|��@@��s���|��@���@�����4vertex_shader_color4��&~���'~��@��)~���*~��@@@���	�#version 330 core
     in vec3 vertex;
     in vec4 color;
     out vec4 v_color;
     void main()
     {
     v_color = color;
     gl_Position = vec4(vertex, 1.0);
     }@��0���1 G��@@@@��3~���4 G��@@��6~���7 G��@���@�����/geometry_shader��B I���C I��@��E I���F I��@@@���	�
     #version 330 core
     layout (points) in;
     layout (points, max_vertices = 1) out;

     
     void main() {
     
     gl_Position = gl_in[0].gl_Position;
     EmitVertex();
     EndPrimitive();
     }
     0@��L J���M V{�@@@@��O I���P V{�@@��R I���S V{�@���@�����/fragment_shader��^ Y���_ Y��@��a Y���b Y��@@@���	b#version 330 core
     in vec4 v_color;
     out vec4 color;
     void main() { color = v_color; }L@��h Z���i ]�@@@@��k Y���l ]�@@��n Y���o ]�@@��qp���r `@@@��tp���u `@��wp���x `@������(Doughnut��� b�� b@�����@�����-vertex_shader��� d*0�� d*=@��� d*0�� d*=@@@������!^��� j���� j��@��� j���� j��@@@��@���	W#version 330 core
     in vec3 vertex;
     in vec3 color;
     out vec4 v_color;
     �@��� e@D�� i��@@@��@������!^��� l���� l��@��� l���� l��@@@��@�����$Util2rotation_functions��� k���� k��@��� k���� k��@@@��@���
  [
       void main() {
       // NOTE https://en.wikipedia.org/wiki/Torus
       vec3 p_model = vertex; // TODO set p_model
       vec4 start = vec4(p_model/2., 1.);
       
       float x_dist = start[0];
       float y_dist = start[1];

       vec4 end =
       RotationX(6.285 * y_dist) *
       Translation(vec3(0, -start[1], -.9)) *
       RotationY(6.285 * x_dist) *
       Translation(vec3(-start[0], 0., .3)) *
       start;


       end = 
       RotationY(3.14 / 2.) * end;

       end = end * 3. / 4.;
       end[3] = 1.;

       v_color = vec4(color, 1.0);
       gl_Position = end;

       }�@��� m���� �
"
+@@@@��� k���� �
"
+@@@@��� e@D�� �
"
+@@@@��� d*,�� �
"
+@@��� d*,�� �
"
+@���@�����/fragment_shader��� �
-
3�� �
-
B@��� �
-
3�� �
-
B@@@�����(Standard/fragment_shader��� �
-
E�� �
-
]@��� �
-
E�� �
-
]@@@@��� �
-
/�� �
-
]@@��� �
-
/�� �
-
]@@��  b"� �
_
b@@@�� b� �
_
b@�� b� �
_
b@������%Eight�� �
d
k� �
d
p@�����@�����-vertex_shader�� �

�� �

�@��! �

��" �

�@@@������!^��+ �
�
��, �
�
�@��. �
�
��/ �
�
�@@@��@���	W#version 330 core
     in vec3 vertex;
     in vec3 color;
     out vec4 v_color;
     @��7 �
�
��8 �
�
�@@@��@������!^��C �
��D �
�@��F �
��G �
�@@@��@�����$Util2rotation_functions��R �
�
��S �
�@��U �
�
��V �
�@@@��@���
  �

         void main() {
         // NOTE https://en.wikipedia.org/wiki/Torus
         vec3 p_model = vertex; // TODO set p_model
         vec4 start = vec4(p_model/2., 1.);
         
         float x_dist = start[0];
         float y_dist = start[1];

         vec4 end =
         RotationX(6.285 * y_dist) *
         Translation(vec3(0, -start[1], -.9)) *
         RotationY(6.285 * x_dist) *
         Translation(vec3(-start[0], 0., .3)) *
         start;


         end = 
         RotationY(3.14 / 2.) * end;

         y_dist = end[1];

         end = RotationY(3.14 * (y_dist + 1.0) / 2.0) * end;

         end = end * 3. / 4.;
         end[3] = 1.;

         v_color = vec4(color, 1.0);
         gl_Position = end;

         }B@��^ ��_ ���@@@@��a �
�
��b ���@@@@��d �
�
��e ���@@@@��g �

��h ���@@��j �

��k ���@���@�����/fragment_shader��v ���w ��@��y ���z ��@@@�����(Standard/fragment_shader��� ���� ��/@��� ���� ��/@@@@��� ���� ��/@@��� ���� ��/@@��� �
d
s�� �14@@@��� �
d
d�� �14@��� �
d
d�� �14@������%White��� �6=�� �6B@�����@�����/fragment_shader��� �LR�� �La@��� �LR�� �La@@@���	i#version 330 core
     out vec4 color;
     void main() { 
     color = vec4(1.0, 1.0, 1.0, 1.0); 
     }�@��� �dh�� ���@@@@��� �LN�� ���@@��� �LN�� ���@@��� �6E�� ���@@@��� �66�� ���@��� �66�� ���@������/Multi_transform��� ����� ���@�����@�����-vertex_shader��� ��� �@��� ��� �@@@���	�
     #version 330 core
     layout (location = 0) in mat4 transformation;

     out VS_OUT {
     mat4 transformation;
     } vs_out;
     
     void main() {
     vs_out.transformation = transpose(transformation);
     }
     �@��� �"&�� �@@@@��� ��� �@@��� ��� �@���@�����/geometry_shader��� ��� �#@��� ��� �#@@@���
  (
     #version 330 core
     layout (points) in;
     layout (triangle_strip, max_vertices = 6) out;

     in VS_OUT {
     mat4 transformation;
     } gs_in[];
     
     void emitTriangle(vec4 a, vec4 b, vec4 c) {
     gl_Position = a;
     EmitVertex();
     gl_Position = b;
     EmitVertex();
     gl_Position = c;
     EmitVertex();
     EndPrimitive();
     }

     void main() {
     
     vec4 a = gs_in[0].transformation * vec4(0.005, 0.0, 0.0, 1.0);
     vec4 b = gs_in[0].transformation * vec4(-0.005, -0.005, 0.0, 1.0);
     vec4 c = gs_in[0].transformation * vec4(-0.005, 0.005, 0.0, 1.0);

     emitTriangle(a, b, c);

     a[0] = a[0] + .3;
     b[0] = b[0] + .3;
     c[0] = c[0] + .3;
     a[1] = -a[1];
     b[1] = -b[1];
     c[1] = -c[1];
     

     emitTriangle(a, b, c);

     }
     �@�� �&*�NT@@@@�� ��NT@@��
 ��NT@���@�����0_geometry_shader��V\�Vl@��V\�Vl@@@���
  �
     #version 330 core
     layout (points) in;
     layout (triangle_strip, max_vertices = 3) out;

     in VS_OUT {
     mat4 transformation;
     } gs_in[];
     
     void main() {
     
     vec4 a = vec4(0.3, 0.1, 0.0, 1.0);
     vec4 b = vec4(0.1, 0.3, 0.0, 1.0);
     vec4 c = vec4(0.1, 0.1, 0.0, 1.0);
     gl_Position = a;
     EmitVertex();
     gl_Position = b;
     EmitVertex();
     gl_Position = c;
     EmitVertex();
     EndPrimitive();
     }
     @�� os�!CI@@@@��#VX�$CI@@��&VX�'CI@���@�����/fragment_shader��2KS�3Kb@��5KS�6Kb@@@�����%White/fragment_shader��?Ke�@Kz@��BKe�CKz@@@@��EKO�FKz@@��HKO�IKz@@��K ���L|@@@��N ����O|@��Q ����R|@������'Circles��[!���\!��@�����@�����-vertex_shader��i"���j"��@��l"���m"��@@@���
  (
    #version 330 core
     in vec2 center;
     in float radius;
     in vec3 color;
     
     out VS_OUT {
     vec2 center;
     float radius;
     vec3 color;
     } vs_out;

     void main() {
     vs_out.center = center;
     vs_out.radius = radius;
     vs_out.color = color;
     }
     W@��s#���t4��@@@@��v"���w4��@@��y"���z4��@������&Filled���6� ��6�@�����@�����/geometry_shader���7��7'@���7��7'@@@���
  �
       #version 330 core
       layout (points) in;
       layout (triangle_strip, max_vertices = 30) out;

       in VS_OUT {
       vec2 center;
       float radius;
       vec3 color;
       } gs_in[];

       out vec4 v_color;

       void main() {
       vec2 center = gs_in[0].center;
       float radius = gs_in[0].radius;
       v_color = vec4(gs_in[0].color, 1.);

       float incr = 3.14 / 30.;
       float counter = 0.;
       for(int i = 0; i < 30; i++) {
       float theta = counter;
       if(i % 2 == 0) {
       theta = -counter;
       }
       gl_Position = vec4(center[0] + radius * cos(theta), center[1] + radius * sin(theta), 0., 1.);
       EmitVertex();
       counter += incr;
       }
       EndPrimitive();
       }
       @���8*0��W#@@@@���7��W#@@���7��W#@@���6�	��X$)@@@���6����X$)@���6����X$)@������(Outlined���Z+4��Z+<@�����@�����/geometry_shader���[FN��[F]@���[FN��[F]@@@���
  �
       #version 330 core
       layout (points) in;
       layout (line_strip, max_vertices = 30) out;

       in VS_OUT {
       vec2 center;
       float radius;
       vec3 color;
       } gs_in[];

       out vec4 v_color;

       void main() {
       vec2 center = gs_in[0].center;
       float radius = gs_in[0].radius;
       v_color = vec4(gs_in[0].color, 1.);

       float incr = 6.28 / 29.;
       float theta = 0.;
       for(int i = 0; i < 30; i++) {
       gl_Position = vec4(center[0] + radius * cos(theta), center[1] + radius * sin(theta), 0., 1.);
       EmitVertex();
       theta += incr;
       }
       EndPrimitive();
       }
       �@���\`f��w��@@@@���[FJ��w��@@���[FJ��w��@@���Z+?��x��@@@���Z+-��x��@���Z+-��x��@������+Color_wheel���z
��z@�����@�����/geometry_shader���{'��{6@���{'��{6@@@���
  �
       #version 330 core
       layout (points) in;
       layout (triangle_strip, max_vertices = 30) out;

       in VS_OUT {
       vec2 center;
       float radius;
       vec3 color;
       } gs_in[];

       out vec4 v_color;

       vec4 colors(int index) {
       index = index % 6;
       vec4 res = vec4(vec3(0.), 1.);
       float comp_strength = 1.;
       float prim_strength = 0.8;
       int start = index / 2;
       if(index % 2 == 0) {
       res[start] = prim_strength;
       } else {
       res[start] = comp_strength;
       res[(start+1)%3] = comp_strength;
       }
       return res;
       }

       void main() {
       vec2 center = gs_in[0].center;
       float radius = gs_in[0].radius;
       v_color = vec4(gs_in[0].color, 1.);

       float incr = 6.283 / 6.;
       for(int i = 0; i < 7; i++) {
       float theta = incr * i;
       vec2 vertex = vec2(cos(theta), sin(theta));
       v_color = colors(i);
       gl_Position = vec4(center[0] + radius * vertex[0], 
       center[1] + radius * vertex[1], 0., 1.);
       EmitVertex();
       v_color = vec4(1.1, 1.1, 1.1, 1.);
       gl_Position = vec4(center[0], center[1], 0., 1.);
       EmitVertex();
       }
       EndPrimitive();
       }�@���|9?���""@@@@�� {#��""@@��{#��""@@��z��""@@@��	z�
�""@��z��""@���@�����/fragment_shader���""��"")@���""��"")@@@�����(Standard/fragment_shader��%�"",�&�""D@��(�"",�)�""D@@@@��+�""�,�""D@@��.�""�/�""D@@��1!���2�"F"I@@@��4!���5�"F"I@��7!���8�"F"I@������'Texture��A�"K"R�B�"K"Y@�����@�����-vertex_shader��O�"c"i�P�"c"v@��R�"c"i�S�"c"v@@@���	�
     #version 330 core
     in vec3 vertex;
     in vec4 color;
     in vec2 tex;

     out vec4 v_color;
     out vec2 v_tex;

     void main() {
     gl_Position = vec4(vertex, 1.);
     v_color = color;
     v_tex = tex;
     }
     =@��Y�"y"}�Z�#f#l@@@@��\�"c"e�]�#f#l@@��_�"c"e�`�#f#l@���@�����/fragment_shader��k�#n#t�l�#n#�@��n�#n#t�o�#n#�@@@���	�
     #version 330 core
     out vec4 color;

     in vec4 v_color;
     in vec2 v_tex;

     uniform sampler2D the_texture;

     void main() {
     color = texture(the_texture, v_tex);
     }
     Y@��u�#�#��v�$M$S@@@@��x�#n#p�y�$M$S@@��{�#n#p�|�$M$S@@��~�"K"\��$T$W@@@����"K"K���$T$W@����"K"K���$T$W@���@�����.compile_shader����$Y$]���$Y$k@����$Y$]���$Y$k@@@��@@���#src����$Y$l���$Y$o@����$Y$l���$Y$o@@@��@@���#typ����$Y$p���$Y$s@����$Y$p���$Y$s@@@��@�����*get_shader����$v$|���$v$�@����$v$|���$v$�@@@��@@���#sid����$v$����$v$�@����$v$����$v$�@@@��@@���!e����$v$����$v$�@����$v$����$v$�@@@�������&Helper'get_int����$v$����$v$�@����$v$����$v$�@@@��@�������"Gl,get_shaderiv����$v$����$v$�@����$v$����$v$�@@@��@����#sid����$v$����$v$�@����$v$����$v$�@@@��@����!e���$v$��	�$v$�@���$v$���$v$�@@@@���$v$���$v$�@����$v$���$v$�@@@@���$v$���$v$�@@@���$v$���$v$�A@@���$v$���$v$�A@@@���$v$x��$v$�@@��@�����#sid��)�$�$��*�$�$�@��,�$�$��-�$�$�@@@�������"Gl-create_shader��8�$�$��9�$�$�@��;�$�$��<�$�$�@@@��@����#typ��E�$�$��F�$�$�@��H�$�$��I�$�$�@@@@��K�$�$��L�$�$�@@@@��N�$�$��O�$�$�@@�  �������"Gl-shader_source��\�$�$��]�$�$�@��_�$�$��`�$�$�@@@��@����#sid��i�$�$��j�$�$�@��l�$�$��m�$�$�@@@��@����#src��v�$�$��w�$�$�@��y�$�$��z�$�$�@@@@��|�$�$��}�$�$�@@@�  �������"Gl.compile_shader����$�$����$�%@����$�$����$�%@@@��@����#sid����$�%���$�%@����$�%���$�%@@@@����$�$����$�%@@@��������!=����%%8���%%9@����%%8���%%9@@@��@������*get_shader����%%���%%!@����%%���%%!@@@��@����#sid����%%"���%%%@����%%"���%%%@@@��@�����"Gl.compile_status����%%&���%%7@����%%&���%%7@@@@����%%���%%7@@@��@�����"Gl%true_����%%:���%%B@����%%:���%%B@@@@����%%���%%B@@@����"Ok����%%H���%%J@�����#sid����%%K���%%N@�� �%%K��%%N@@@���%%H��%%N@@@���@�����#len���%T%\��%T%_@���%T%\��%T%_@@@������*get_shader���%T%b��%T%l@���%T%b� �%T%l@@@��@����#sid��)�%T%m�*�%T%p@��,�%T%m�-�%T%p@@@��@�����"Gl/info_log_length��8�%T%q�9�%T%�@��;�%T%q�<�%T%�@@@@��>�%T%b�?�%T%�@@@@��A�%T%X�B�%T%�@@��@�����#log��L�%�%��M�%�%�@��O�%�%��P�%�%�@@@�������&Helper*get_string��[�%�%��\�%�%�@��^�%�%��_�%�%�@@@��@����#len��h�%�%��i�%�%�@��k�%�%��l�%�%�@@@��@�������"Gl3get_shader_info_log��y�%�%��z�%�%�@��|�%�%��}�%�%�@@@��@����#sid����%�%����%�%�@����%�%����%�%�@@@��@����#len����%�%����%�%�@����%�%����%�%�@@@��@����$None����%�%����%�%�@@����%�%����%�%�@@@@����%�%����%�%�@�����%�%����%�%�@@@@����%�%����%�%�@@@@����%�%����%�%�@@�  �������"Gl-delete_shader����%�%����%�&	@����%�%����%�&	@@@��@����#sid����%�&
���%�&@����%�&
���%�&@@@@����%�%����%�&@@@����%Error����%�&���%�&@���#Msg�����#log����%�&���%�&@����%�&���%�&@@@����%�&���%�&@�����%�&���%�&@@@����%�&���%�&@@@����%�%����%�& @�����%�%����%�&@@@����%�%����%�& @@@�� �%T%X��%�& @@@���%%��%�& @@@���$�$���%�& @@@��	�$�$��
�%�& @@@���$�$���%�& @@@���$v$x��%�& @@@���$Y$p��%�& A@@���$Y$l��%�& A@@@���$Y$Y��%�& @@���$Y$Y��%�& @���@��������'�%�& �(�%�& @@��*�%�& �+�%�& @@@����������)unset_lib��6�%�& �7�%�& @��9�%�& �:�%�& @@@��@���"%@��A�%�& �B�%�& @@@@��D�%�& �E�%�& @@@@��G�%�& �H�%�& @@��J�%�& �K�%�& @���@������!��V�%�& �W�%�& @@��Y�%�& �Z�%�& @@@�������� %unset��e�%�& �f�%�& @��h�%�& �i�%�& @@@��@����"()��r�%�& �s�%�& @@��u�%�& �v�%�& @@@@��x�%�& �y�%�& @@@@��{�%�& �|�%�& @@��~�%�& ��%�& @���@�����������%�& ���%�& @@����%�& ���%�& @@@������������%unset����%�& ���%�& @����%�& ���%�& @@@��@���������%�& ���%�& @@����%�& ���%�& @@@@����%�& ���%�& @@@@����%�& ���%�& @@����%�& ���%�& @@