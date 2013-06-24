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
    

    let loadAsset path auxFlags =         
        let importer = new AssimpImporter()   
        let importedAsset = importer.ImportFile(path, PostProcessSteps.Triangulate            
            ||| PostProcessSteps.PreTransformVertices 
            ||| auxFlags)               
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
    
    let rtSurface (device:SharpDX.Direct3D11.Device) (w, h) (samples:SampleDescription) (format:Format) = 
        let desc = new Texture2DDescription(Width = w, Height=h, Format=format,
                    SampleDescription=samples, Usage=ResourceUsage.Default, BindFlags=(BindFlags.RenderTarget|||BindFlags.ShaderResource), 
                    ArraySize=1, MipLevels=1)
        let tex = new Texture2D(device, desc)
        tex, new RenderTargetView(device, tex), new ShaderResourceView(device, tex)

    

    let depthSurface (device:SharpDX.Direct3D11.Device) (w, h) (samples:SampleDescription) =     
        let texDesc = new Texture2DDescription(Width = w, Height=h, Format=Format.R32_Typeless,
                        SampleDescription=samples, Usage=ResourceUsage.Default, 
                        BindFlags=(BindFlags.DepthStencil|||BindFlags.ShaderResource), 
                        ArraySize=1, MipLevels=1)
        let tex = new Texture2D(device, texDesc)

        let dsvDimension, srvDimension = 
            match samples.Count with
            | 1 -> DepthStencilViewDimension.Texture2D, ShaderResourceViewDimension.Texture2D
            | _ -> DepthStencilViewDimension.Texture2DMultisampled, ShaderResourceViewDimension.Texture2DMultisampled

        let dsvDesc = DepthStencilViewDescription(Format=Format.D32_Float,
                            Flags = DepthStencilViewFlags.None,
                            Dimension= dsvDimension)
        
        let srvDesc = ShaderResourceViewDescription(Format=Format.R32_Float,
                            Dimension= srvDimension,
                            Texture2D=ShaderResourceViewDescription.Texture2DResource(MipLevels=1))
            
        tex, new DepthStencilView(device, tex, dsvDesc), new ShaderResourceView(device, tex, srvDesc)

    let swapChainDesc window sampleDesc = 
        let (w, h) = size window
        let modeDesc = new ModeDescription(w, h, new Rational(60, 1), Format.R8G8B8A8_UNorm)
        new SwapChainDescription(
            BufferCount = 1,
            ModeDescription = modeDesc,
            IsWindowed = sdx_true,
            OutputHandle = window.Handle,
            SampleDescription = sampleDesc,
            SwapEffect = SwapEffect.Discard,
            Usage = Usage.RenderTargetOutput
            )
    let deviceAndSwapChain window sampleDesc = 
        Device.CreateWithSwapChain(
            DriverType.Hardware, 
            DeviceCreationFlags.Debug,
            swapChainDesc window sampleDesc)

    let rtv device tex = new RenderTargetView(device, tex)

    let shaderByteCode file funcName version = 
        ShaderBytecode.CompileFromFile(file, funcName, version, 
            ShaderFlags.Debug ||| ShaderFlags.SkipOptimization ||| ShaderFlags.SkipOptimization, EffectFlags.None).Bytecode.Data
    let maxDepth = 1.f
    let clear (ctx : DeviceContext) (rtvs:RenderTargetView[]) (dsv:DepthStencilView) = 
        ctx.ClearDepthStencilView(dsv, DepthStencilClearFlags.Depth, maxDepth, 0uy)
        for rtv in rtvs do
            ctx.ClearRenderTargetView(rtv, new Color4(1.f, 0.f, 1.f, 1.f))
        

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

    let prepareOutputMerger (ctx:DeviceContext) (rtv:RenderTargetView[]) (dsv:option<DepthStencilView>) = 
        match dsv with 
        | Some x -> ctx.OutputMerger.SetTargets(x, rtv)
        | None -> ctx.OutputMerger.SetTargets(rtv)
    let clearOutputs ctx = prepareOutputMerger ctx [|null; null; null; null|] (Some null)
    let clearResources (ctx:Direct3D11.DeviceContext) = 
        ctx.VertexShader.SetShaderResources(0, [|null; null; null; null; null; null; null|])
        ctx.PixelShader.SetShaderResources(0, [|null; null; null; null; null; null; null|])

    let updateCb (ctx:DeviceContext) (buf:CBuffer) data = 
        ctx.UpdateSubresource(ref data, buf.DxBuffer)

    let prepareWindow (window:System.Windows.Forms.Form) (device:Direct3D11.Device) (swapChain:SwapChain) = 
        let factory = swapChain.GetParent<Factory>()
        factory.MakeWindowAssociation(window.Handle, WindowAssociationFlags.IgnoreAll)
        let backbuffer = Texture2D.FromSwapChain<Texture2D>(swapChain, 0)
        let backbufferRtv = rtv device backbuffer
        (backbuffer, backbufferRtv)

    let prepareShader setShader setCbs setSrvs shader (cbs:CBuffer[]) (srvs:ShaderResourceView[])= 
        setShader shader
        if cbs.Length > 0 then
            setCbs (cbs |> Array.map (fun x -> x.DxBuffer))
        if srvs.Length > 0 then
            setSrvs srvs

    let prepareVs (ctx : DeviceContext) vs cbs srvs = 
        prepareShader ctx.VertexShader.Set 
            (fun x -> ctx.VertexShader.SetConstantBuffers(0, x)) (fun x -> ctx.VertexShader.SetShaderResources(0, x))
            vs cbs srvs
        
    let preparePs (ctx : DeviceContext) ps cbs srvs = 
        prepareShader ctx.PixelShader.Set 
            (fun x -> ctx.PixelShader.SetConstantBuffers(0, x)) (fun x -> ctx.PixelShader.SetShaderResources(0, x))
            ps cbs srvs

module gfx = 
    open d3d
    type ReadSurface = Texture2D * ShaderResourceView
    type WriteSurface = Texture2D * RenderTargetView
    type Backbuffer = Backbuffer of WriteSurface
    type ReadWriteSurface = Texture2D * ShaderResourceView * RenderTargetView
    type DepthSurface = Texture2D * ShaderResourceView * DepthStencilView 

    type GfxSystem = {Device:SharpDX.Direct3D11.Device; Context:SharpDX.Direct3D11.DeviceContext}
    type Msaa = Msaa of int
        
    type VertexShaderState = {
        Shader: VertexShader;
        Resources: ShaderResourceView[];
        Constants: CBuffer[];
        InputLayout : InputLayout;
    }
    type PixelShaderState = {
        Shader: PixelShader;
        Resources: ShaderResourceView[];
        Constants: CBuffer[];
    }
    type OutputState = {    
        Depth: option<DepthStencilView>;
        RenderTargets: RenderTargetView[];
        Size: float32*float32
    }
    type PipelineState = {
        VertexShader: VertexShaderState;
        PixelShader: PixelShaderState;
        Output: OutputState;
        VertexBuffer : VertexBuffer;
        IndexBuffer : IndexBuffer;  
    }
    
    type DrawOperation = {
        State : PipelineState;      
    }
    
    let applyState system state = 
        clearOutputs system.Context
        clearResources system.Context

        let {VertexShaderState.Shader=vs; Resources=vsResources; Constants=vsCb; InputLayout=il} = state.VertexShader
        let {PixelShaderState.Shader=ps; Resources=psResources; Constants=psCb} = state.PixelShader
        let {Depth=depth; RenderTargets=rtvs; Size=size} = state.Output
        let vb, ib = state.VertexBuffer, state.IndexBuffer
        let {Device=device; Context=ctx} = system

        prepareInputAssembler ctx il vb ib
        prepareVs ctx vs vsCb vsResources
        preparePs ctx ps psCb psResources
        prepareRasterizer ctx size 
        prepareOutputMerger ctx rtvs depth        

    //this api abstraction system is very leaky... refactor needed if we want ogl
    //we ideally want a api independent way to specify a draw op, and then conver that to api specific draw op
    //for now it's all prototype so fine
    let draw system (drawop:DrawOperation) = 
        applyState system drawop.State
        let _, ib = drawop.State.VertexBuffer, drawop.State.IndexBuffer
        let {Device=device; Context=ctx} = system
        ctx.DrawIndexed(int(ib.IndicesCount), 0, 0)
            
            
    let dxSampleDesc (Msaa msaaCount) = (new SampleDescription(msaaCount, 0))

    let makeRgba16 {Device=device} size msaa = 
        let tex, rtv, srv = rtSurface device size (dxSampleDesc msaa) Format.R16G16B16A16_Float
        tex, srv, rtv
    let makeD32 {Device=device} size msaa = 
        let tex, dsv, srv = depthSurface device size (dxSampleDesc msaa) 
        tex, srv, dsv
               
module quad = 
   open d3d 
   open lib3d
   let quadInputElements = [|
        {Name = "POSITION"; Index = 0; Format = Format.R32G32B32_Float; Slot = 0; AlignedByteOffset = InputElement.AppendAligned} 
   |]
   let quadVertices = 
       [|
            -1; 1; 0; //upper left
            1; 1; 0; //upper right
            1; -1; 0; //lower right
            -1; -1; 0; //lower left
       |] 
       |> Array.map (fun x -> float32(x)) 
   let quadIndices = [|0; 1; 2; 2; 3; 0|] |> Array.map (fun x -> uint32(x))
   let quadVbIb device = 
        triVertexBuffer device quadVertices (StrideInBytes(uint32(3 * 4))) quadIndices
   let quadInputLayout device vsBytecode = layout device vsBytecode quadInputElements     

module render = 
    open gfx
    open lib3d
    open d3d //not really well isolated atm... but that's fine
    open quad
    type GBuffer = {Albedo: ReadWriteSurface; Normal: ReadWriteSurface; Depth: DepthSurface}  

    type ShaderState = { VertexShader: VertexShader; PixelShader:PixelShader; InputLayout:InputLayout}
    type GenGBuferCBuffers = {ViewProjCb : CBuffer}
    type ShadeGBufferQuad = { VertexBuffer: VertexBuffer; IndexBuffer: IndexBuffer}
    type GBufferShaderState = { GenGBuffer: ShaderState * GenGBuferCBuffers; ShadeGBuffer: ShaderState*ShadeGBufferQuad}
    
    
    let gbufferInputElements = [
        {Name = "POSITION"; Index = 0; Format = Format.R32G32B32_Float; Slot = 0; AlignedByteOffset = InputElement.AppendAligned} 
        {Name = "NORMAL"; Index = 0; Format = Format.R32G32B32_Float; Slot = 0; AlignedByteOffset = InputElement.AppendAligned}
    ]
    let clear (ctx:DeviceContext) {Albedo=(_,_,albedoRtv); Normal=(_,_,normalRtv); Depth=(_,_,dsv)} =
        clear ctx [|albedoRtv; normalRtv|] dsv
    let makeGBufferState device = 
        let genGBufferShaderState = 
            let vs, vsBytecode = vs device "genGBuffer.fx"
            { ShaderState.VertexShader = vs
              PixelShader = fst (ps device "genGBuffer.fx")
              InputLayout = layout device vsBytecode gbufferInputElements
            }, {ViewProjCb = cb device sizeof<Matrix>}
        let shadeGBufferShaderState = 
            let vs, vsBytecode = vs device "shadeGBuffer.fx"
            let ib, vb = quadVbIb device
            { ShaderState.VertexShader = vs
              PixelShader = fst (ps device "shadeGBuffer.fx")
              InputLayout = quadInputLayout device vsBytecode
            }, {VertexBuffer = vb; IndexBuffer = ib}
        {GenGBuffer = genGBufferShaderState; ShadeGBuffer = shadeGBufferShaderState}
            

    let makeGBuffer system size msaa = 
        {Albedo=makeRgba16 system size msaa; 
        Normal=makeRgba16 system size msaa; 
        Depth=makeD32 system size msaa}

    let genGBufferPass system {Albedo=(_,_,albedoRtv); Normal=(_,_,normalRtv); Depth=(depthTex, _, dsv)} 
        //WILL MODIFY the cbuffer!!!
        {GenGBuffer={VertexShader=vs; PixelShader=ps; InputLayout=il}, {ViewProjCb=vpCb}} size cam (vb, ib) = 
        let vpMat = viewProjMatrix cam        
        updateCb system.Context vpCb vpMat        

        let pipelineState = { 
                        VertexShader = { Shader = vs; Resources = [||]; Constants = [|vpCb|]; InputLayout = il; };
                        PixelShader = { Shader = ps; Resources = [||]; Constants = [||]; };                    
                        Output = { 
                                    Depth = Some dsv;
                                    RenderTargets = [|albedoRtv; normalRtv|];
                                    Size = size;
                        };
                        VertexBuffer = vb; 
                        IndexBuffer = ib
                }
        draw system { State = pipelineState; }

    let shadeGBufferPass system {Albedo=(_,albedoSrv,_); Normal=(_,normalSrv,_);} 
        {ShadeGBuffer={VertexShader=vs; PixelShader=ps; InputLayout=il}, {VertexBuffer=vb; IndexBuffer=ib}}
        (Backbuffer(_,backBufferRtv)) size =       
          
        let pipelineState = { 
                        VertexShader = { Shader = vs; Resources = [||]; Constants = [||]; InputLayout = il; };
                        PixelShader = { Shader = ps; Resources = [|albedoSrv; normalSrv|]; Constants = [||]; };                    
                        Output = { Depth = None; RenderTargets = [|backBufferRtv|]; Size = size; };
                        VertexBuffer = vb; 
                        IndexBuffer = ib;
                }
        draw system { State = pipelineState; }
                
    
open lib3d
open d3d
open assets
open lighting
open libcolor
open input
open gfx
open render
[<EntryPoint>]
let main argv = 

    //TODO: keep in sync
    let sampleDesc = new SampleDescription(1, 0)
    let msaa = Msaa 1


    //let models, moveSpeed = loadAsset "../../asset_obj/sponza.obj" PostProcessSteps.None, 0.2f
    let models, moveSpeed = loadAsset "../../nff/sphere.nff" PostProcessSteps.GenerateSmoothNormals, 0.0002f


    let window = new RenderForm("drawlib!")
    window.MaximumSize <- new Drawing.Size(800,800)
    window.MinimumSize <- new Drawing.Size(800,800)
    let (device, swapChain) = deviceAndSwapChain window sampleDesc
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
    let immediateCtx = device.ImmediateContext
    
    let lights = [|
        AmbientLight {Color=Rgb(0.8f, 0.6f, 0.9f); Intensity=1.0f}
    |]   
    
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
    let aPressed = myKeyPressed Key.A
    let dPressed = myKeyPressed Key.D
    let system = {Device=device; Context=immediateCtx;}
    let gbufferState = makeGBufferState device
    let gbuffer = makeGBuffer system (size window) msaa
    RenderLoop.Run(window, (fun () ->    
            let _,_,dsv = gbuffer.Depth
            render.clear immediateCtx gbuffer

            if wPressed() then cam := move !cam {x=0.f;y=0.f;z=moveSpeed}
            if sPressed() then cam := move !cam {x=0.f;y=0.f;z= -moveSpeed}
            if aPressed() then cam := move !cam {x= -moveSpeed;y=0.f;z=0.f}
            if dPressed() then cam := move !cam {x=moveSpeed;y=0.f;z=0.f}

            for (ib, vb) in ibVbs do                         
                genGBufferPass system gbuffer gbufferState (sizef window) !cam (vb, ib)

            shadeGBufferPass system gbuffer gbufferState (Backbuffer(backbuffer, backbufferRtv)) (sizef window)             
            swapChain.Present(0, PresentFlags.None)            
        ))
        
    0 // return an integer exit code
