// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
#light

open System
open SharpDX
open SharpDX.D3DCompiler
open SharpDX.Direct3D
open SharpDX.Direct3D11
open SharpDX.DXGI
open SharpDX.Windows
open Assimp
module libcolor = 
    type Color = 
        | Rgb of float32*float32*float32
        | Hsv of float32*float32*float32
        
    let toRgb color = 
        match color with
        | Rgb(_,_,_) -> color
        | Hsv(_,_,_) -> failwith "not implemented"
        
module lib3d = 
    type vec4 = {x:float32; y:float32; z:float32; w:float32}
    type vec3 = {x:float32; y:float32; z:float32}
    type Camera = {Eye:vec3; Forward:vec3; Up:vec3; Fov:float32; AR:float32; ZNear:float32; ZFar:float32}
    type PositionDirection = {Position : vec3; Direction: vec3}
    
    let toNativeVec3 ({vec3.x=x; y=y; z=z}) = new Vector3(x,y,z)
    let toNativeVec4 ({vec4.x=x; y=y; z=z; w=w}) = new Vector4(x,y,z,w)
    let toMyVec3 (v:Vector3) = {vec3.x=v.X; y=v.Y; z=v.Z}
    let viewMatrix {Eye=eye; Forward=forward; Up=up} = 
        Matrix.LookAtLH(toNativeVec3(eye), Vector3.Add(toNativeVec3(eye), toNativeVec3(forward)), toNativeVec3(up))
    let projMatrix {Fov=fov; AR=ar; ZNear=zNear; ZFar=zFar} = Matrix.PerspectiveFovLH(fov, ar, zNear, zFar)

    let viewProjMatrix cam = Matrix.Multiply((viewMatrix cam), (projMatrix cam))
    
    let move cam dp = 
        let newPosition = Vector3.Add((toNativeVec3 cam.Eye),(toNativeVec3 dp))
        {cam with Eye = toMyVec3 newPosition}
        
    type StrideInBytes = StrideInBytes of uint32
    type SizeInBytes = SizeInBytes of uint32
    type Offset = Offset of int32    
    let numElements (StrideInBytes(stride)) (SizeInBytes(size)) = size / stride
        
    
    
module lighting = 
    open lib3d
    open libcolor
    type Radiance = {Color:Color; Intensity:float32} //is it really called radiance?
    type Light = 
        | DirectionalLight of PositionDirection * Radiance
        | AmbientLight of Radiance

module assets = 
    open lib3d
    type Vertex = {Position:vec3; Normal:vec3}
    type Geometry = { Indices: uint32[]; Vertices : Vertex[] }
    

    let objAsset path = 
        let importer = new AssimpImporter()        
        let importedAsset = importer.ImportFile(path, PostProcessSteps.Triangulate
            ///||| PostProcessSteps.GenerateNormals
            ||| PostProcessSteps.PreTransformVertices 
            )        
        importedAsset.Meshes 
        |> Array.map (
            fun mesh -> 
                {Vertices = mesh.Vertices |> Array.zip mesh.Normals 
                    |> Array.map (
                        fun (n, p) -> {Position={x=p.X;y=p.Y;z=p.Z}; Normal={x=n.X;y=n.Y;z=n.Z}};);
                    Indices = mesh.GetIndices()})
module d3d = 
    open lib3d
    open assets
    let vertexByteSize = StrideInBytes(uint32(3 (* vec3 *) * 4 (* sizeof(float) *) * 2 (* pos *)))
    type VsInputElement = { 
        Name: string; 
        Index : int;
        Format : Format;
        AlignedByteOffset : int;
        Slot : int;
    }    

    type VertexBuffer = { DxBuffer : Buffer; Stride: StrideInBytes; Offset: Offset; VertexCount : uint32 }
    type IndexBuffer = { DxBuffer: Buffer; Stride: StrideInBytes; Offset: Offset; IndicesCount: uint32; Format : Format }
    type CBuffer = { DxBuffer: Buffer; }
    type VSByteCode = VSByteCode of byte[]
    type PSByteCode = PSByteCode of byte[]

    let sdx_false = new SharpDX.Bool(false)
    let sdx_true = new SharpDX.Bool(true) //this might not be right...

    let sizef (window : System.Windows.Forms.Form) = 
        (float32 window.ClientSize.Width, float32 window.ClientSize.Height)

    let size (window : System.Windows.Forms.Form) = 
        (window.ClientSize.Width, window.ClientSize.Height)
    
    let swapChainDesc window = 
        let (w, h) = size window
        let modeDesc = new ModeDescription(w, h, new Rational(60, 1), Format.R8G8B8A8_UNorm)
        new SwapChainDescription(
            BufferCount = 1,
            ModeDescription = modeDesc,
            IsWindowed = sdx_true,
            OutputHandle = window.Handle,
            SampleDescription = new SampleDescription(8, 0),
            SwapEffect = SwapEffect.Discard,
            Usage = Usage.RenderTargetOutput
            )
    let deviceAndSwapChain window = 
        Device.CreateWithSwapChain(
            DriverType.Hardware, 
            DeviceCreationFlags.None,
            swapChainDesc window)

    let rtv device tex = new RenderTargetView(device, tex)

    let shaderByteCode file funcName version = 
        ShaderBytecode.CompileFromFile(file, funcName, version, 
            ShaderFlags.Debug ||| ShaderFlags.SkipOptimization ||| ShaderFlags.SkipOptimization, EffectFlags.None).Bytecode.Data


    let vs device file = 
        let bytecode = shaderByteCode file "VS" "vs_5_0"
        (new VertexShader(device, bytecode), VSByteCode bytecode)
    
    let ps device file = 
        let bytecode = shaderByteCode file "PS" "ps_5_0"
        (new PixelShader(device, bytecode), PSByteCode bytecode)

    let layout device (VSByteCode vsByteCode) (inputs:seq<VsInputElement>) = 
        let realInputs = 
            inputs 
            |> Seq.map (fun x -> 
                new InputElement(x.Name, x.Index, x.Format, x.AlignedByteOffset, x.Slot))
            |> Seq.toArray
        new InputLayout(device, ShaderSignature.GetInputSignature(vsByteCode).Data, realInputs)
    let byteSize (_:vec4) = 16
    

    let triVertexBuffer device (vertexData:float32[]) vertexStrideInBytes (indices:uint32[])= 
        let toSDXVec4 {x=x;y=y;z=z;w=w;} = new Vector4(x,y,z,w)
        let toSDXVec3 {vec3.x=x;y=y;z=z;} = new Vector3(x,y,z)
        
        let ib = { DxBuffer = Buffer.Create(device, BindFlags.IndexBuffer, indices); 
            Stride = StrideInBytes (uint32(sizeof<uint32>)); 
            Offset = Offset 0; 
            IndicesCount = uint32(indices.Length);
            Format = Format.R32_UInt }
        let vb = { DxBuffer = Buffer.Create(device, BindFlags.VertexBuffer, vertexData); 
            Stride = vertexStrideInBytes;
            Offset = Offset 0; 
            VertexCount = numElements vertexStrideInBytes (SizeInBytes(uint32(vertexData.Length * 4))) }
        (ib, vb)

    let cb device size = 
        {DxBuffer = new Buffer(device, size, ResourceUsage.Default, 
                                BindFlags.ConstantBuffer, CpuAccessFlags.None, ResourceOptionFlags.None, 0)}
    
    let prepareInputAssembler (ctx : DeviceContext) layout 
        ({Offset=Offset(vbOffset);Stride=StrideInBytes(vbStride)} as vb:VertexBuffer) 
        ({Offset=Offset(ibOffset);} as ib:IndexBuffer) = 
        ctx.InputAssembler.InputLayout <- layout
        ctx.InputAssembler.PrimitiveTopology <- PrimitiveTopology.TriangleList
        ctx.InputAssembler.SetVertexBuffers(0, new VertexBufferBinding(Buffer = vb.DxBuffer, Offset = vbOffset, Stride = int(vbStride)))
        ctx.InputAssembler.SetIndexBuffer(ib.DxBuffer, ib.Format, ibOffset)

    let prepareRasterizer (ctx:DeviceContext) (w,h)= 
        ctx.Rasterizer.SetViewport(0.f, 0.f, w, h, 0.f, 1.f)

    let prepareOutputMerger (ctx:DeviceContext) (rtv:RenderTargetView) = 
        ctx.OutputMerger.SetTargets(rtv)

    let updateCb (ctx:DeviceContext) (buf:CBuffer) data = 
        ctx.UpdateSubresource(ref data, buf.DxBuffer)

    let prepareWindow (window:System.Windows.Forms.Form) (device:Direct3D11.Device) (swapChain:SwapChain) = 
        let factory = swapChain.GetParent<Factory>()
        factory.MakeWindowAssociation(window.Handle, WindowAssociationFlags.IgnoreAll)
        let backbuffer = Texture2D.FromSwapChain<Texture2D>(swapChain, 0)
        let backbufferRtv = rtv device backbuffer
        (backbuffer, backbufferRtv)

    let prepareShader setShader setCbs shader (cbs:CBuffer[]) = 
        setShader shader
        if cbs.Length > 0 then
            setCbs (cbs |> Array.map (fun x -> x.DxBuffer))

    let prepareVs (ctx : DeviceContext) vs cbs = 
        prepareShader ctx.VertexShader.Set (fun x -> ctx.VertexShader.SetConstantBuffers(0, x)) vs cbs
        
    let preparePs (ctx : DeviceContext) ps cbs = 
        prepareShader ctx.PixelShader.Set (fun x -> ctx.PixelShader.SetConstantBuffers(0, x)) ps cbs
module gfx = 
    open d3d
    type Rgba32 = 
        | DxRgba32 of Texture2D*option<RenderTargetView>*option<ShaderResourceView>
    type Rgb32 = 
        | DxRgb32 of Texture2D*option<RenderTargetView>*option<ShaderResourceView>
    type Surface = 
        | Rgba32 of Rgba32
        | Rgb32 of Rgb32
module input = 
    open SharpDX.DirectInput
    type Key = | W | A | S | D
    let keyMap = [| 
        A, DirectInput.Key.A;
        S, DirectInput.Key.S;
        W, DirectInput.Key.W;
        D, DirectInput.Key.D;
    |]
    let toKey (key:DirectInput.Key) = 
        keyMap |> Array.tryPick (fun (myKey, dxKey) -> match dxKey with | k when k = key -> Some myKey | _ -> None)

    let pollingInput (form:System.Windows.Forms.Form) = 
        let di = new DirectInput()
        let kb = new Keyboard(di)
        let mouse = new Mouse(di)   
        let init () = 
            kb.SetCooperativeLevel(form, CooperativeLevel.Foreground ||| CooperativeLevel.NonExclusive)
            mouse.SetCooperativeLevel(form, CooperativeLevel.Foreground ||| CooperativeLevel.NonExclusive)
            mouse.Acquire()        
            kb.Acquire()
        let kbFunc () = 
            try kb.GetCurrentState().PressedKeys.ToArray()
            with _ -> [||]
            |> Array.map toKey |> Array.choose id
        let mousePos = ref (0, 0)
        let mouseFunc () = 
            try 
                let state = mouse.GetCurrentState()
                let dx, dy = state.X - fst(!mousePos), state.Y - snd(!mousePos)
                mousePos := (state.X, state.Y)
                (dx, dy)
            with _ -> (0, 0)
        kbFunc, mouseFunc, init
    let keyPressed (poller:unit->Key[]) (key:Key) () = poller () |> Array.exists (fun x -> x = key)


module render = 
    open gfx
    type GBuffer = {Albedo: Rgb32; Normal: Rgb32;}    
    
open lib3d
open d3d

let myInputElements = [
    {Name = "POSITION"; Index = 0; Format = Format.R32G32B32_Float; Slot = 0; AlignedByteOffset = InputElement.AppendAligned} 
    {Name = "NORMAL"; Index = 0; Format = Format.R32G32B32_Float; Slot = 0; AlignedByteOffset = InputElement.AppendAligned}
]
open assets
open lighting
open libcolor
open input
[<EntryPoint>]
let main argv = 

    let models = objAsset "../../asset_obj/sponza.obj"
    //let models = objAsset "../../nff/sphere.nff"

    let window = new RenderForm("drawlib!")
    window.MaximumSize <- new Drawing.Size(800,800)
    window.MinimumSize <- new Drawing.Size(800,800)
    let (device, swapChain) = deviceAndSwapChain window
    let (backbuffer, backbufferRtv) = prepareWindow window device swapChain
    let simpleVs, vsByteCode = vs device "MiniTri.fx"
    let simplePs, _ = ps device "MiniTri.fx"
    let rand = new Random()
    printfn "start making ib, vb"
    let flattenVertex (vertices:Vertex[]) = 
        vertices 
        |> Array.collect (fun {Position={x=px;y=py;z=pz};Normal={x=nx;y=ny;z=nz}} -> 
            [|px;py;pz;nx;ny;nz;|]) 
    
    let ibVbs = models |> Array.map (fun x -> triVertexBuffer device (flattenVertex x.Vertices) vertexByteSize x.Indices)
    printfn "end making ib, vb"
    let myLayout = layout device vsByteCode myInputElements 
    let immediateCtx = device.ImmediateContext
    
    let lights = [|
        AmbientLight {Color=Rgb(0.8f, 0.6f, 0.9f); Intensity=1.0f}
    |]
    
    let vpCb = cb device sizeof<Matrix>

    let cam = ref {Camera.Eye={x=0.f;y=0.f;z= -1.f}; 
        Forward={x=0.f;y=0.f;z=1.f};
        Up={x=0.f;y=1.f;z=0.f;};
        Fov = 3.14f / 2.f;
        AR = 1.f;
        ZNear = 0.1f;
        ZFar = 10000.f
    }

    let pollKb, pollMouse, pollInit = pollingInput window
    window.Show()
    pollInit ()
    let myKeyPressed = keyPressed pollKb
    let wPressed = myKeyPressed Key.W
    let sPressed = myKeyPressed Key.S

    RenderLoop.Run(window, (fun () ->    
            let clearColor = new Color4(Color.Black.ToColor3())
            immediateCtx.ClearRenderTargetView(backbufferRtv, clearColor)

            if wPressed() then cam := move !cam {x=0.02f;y=0.02f;z= 0.02f}
            if sPressed() then cam := move !cam {x= -0.02f;y= -0.02f;z= -0.02f}

            let vpMat = viewProjMatrix !cam

            updateCb immediateCtx vpCb vpMat

            for (ib, vb) in ibVbs do  
                        
                prepareInputAssembler immediateCtx myLayout vb ib
                prepareRasterizer immediateCtx (sizef window) 
                prepareOutputMerger immediateCtx backbufferRtv
                prepareVs immediateCtx simpleVs [|vpCb|]
                preparePs immediateCtx simplePs [||]

                immediateCtx.DrawIndexed(int(ib.IndicesCount), 0, 0)
            swapChain.Present(0, PresentFlags.None)            
        ))
        
    0 // return an integer exit code
