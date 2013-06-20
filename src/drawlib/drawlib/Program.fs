﻿// Learn more about F# at http://fsharp.net
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
module assets = 
    type vec3 = float32 * float32 * float32
    type Geometry = { Indices: uint32[]; Vertices : vec3[] }

    let objAsset path = 
        let importer = new AssimpImporter()        
        let importedAsset = importer.ImportFile(path, PostProcessSteps.PreTransformVertices 
            ||| PostProcessSteps.Triangulate
            ||| PostProcessSteps.FlipWindingOrder)        
        importedAsset.Meshes 
        |> Array.map (
            fun mesh -> 
                {Vertices = mesh.Vertices 
                    |> Array.map (
                        fun vertices -> (vertices.X, vertices.Y, vertices.Z));
                    Indices = mesh.GetIndices()})
        
                                 
        
module lib3d = 
    type VsInputElement = { 
        Name: string; 
        Index : int;
        Format : Format;
        AlignedByteOffset : int;
        Slot : int;
    }
    type vec4 = float32*float32*float32*float32    
    type VertexBuffer = { DxBuffer : Buffer; Stride: int32; Offset: int32; VertexCount : int32 }
    type IndexBuffer = { DxBuffer: Buffer; Stride: int32; Offset: Int32; IndicesCount: int32; Format : Format }
    
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
            SampleDescription = new SampleDescription(1, 0),
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
        ShaderBytecode.CompileFromFile(file, funcName, version, ShaderFlags.Debug ||| ShaderFlags.SkipOptimization, EffectFlags.None).Bytecode.Data


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



    let triVertexBuffer device (vertices:vec4[][]) (indices:uint32[])= 
        let toSDXVec4 (x,y,z,w) = new Vector4(x,y,z,w)
        let flattened = vertices |> Array.collect (fun (els:array<vec4>) -> els |> Array.map toSDXVec4)
        
        let ib = { DxBuffer = Buffer.Create(device, BindFlags.IndexBuffer, indices); 
            Stride = sizeof<uint32>; 
            Offset = 0; 
            IndicesCount = indices.Length;
            Format = Format.R32_UInt }
        let vb = { DxBuffer = Buffer.Create(device, BindFlags.VertexBuffer, flattened); 
            Stride = vertices.[0].Length * (byteSize vertices.[0].[0]); 
            Offset = 0; 
            VertexCount = vertices.Length }
        (ib, vb)

    let prepareInputAssembler (ctx : DeviceContext) layout (vb:VertexBuffer) (ib:IndexBuffer) = 
        ctx.InputAssembler.InputLayout <- layout
        ctx.InputAssembler.PrimitiveTopology <- PrimitiveTopology.TriangleList
        ctx.InputAssembler.SetVertexBuffers(0, new VertexBufferBinding(Buffer = vb.DxBuffer, Offset = vb.Offset, Stride = vb.Stride))
        ctx.InputAssembler.SetIndexBuffer(ib.DxBuffer, ib.Format, ib.Offset)

    let prepareRasterizer (ctx:DeviceContext) (w,h)= 
        ctx.Rasterizer.SetViewport(0.f, 0.f, w, h, 0.f, 1.f)

    let prepareOutputMerger (ctx:DeviceContext) (rtv:RenderTargetView) = 
        ctx.OutputMerger.SetTargets(rtv)

    let prepareWindow (window:System.Windows.Forms.Form) (device:Direct3D11.Device) (swapChain:SwapChain) = 
        let factory = swapChain.GetParent<Factory>()
        factory.MakeWindowAssociation(window.Handle, WindowAssociationFlags.IgnoreAll)
        let backbuffer = Texture2D.FromSwapChain<Texture2D>(swapChain, 0)
        let backbufferRtv = rtv device backbuffer
        (backbuffer, backbufferRtv)
    let prepareShaders (ctx : DeviceContext) vs ps =  
        ctx.VertexShader.Set(vs)
        ctx.PixelShader.Set(ps)
        

open lib3d
let myVertices = [|
    [|(0.0f, 0.5f, 0.5f, 1.0f); (1.0f, 0.0f, 0.0f, 1.0f)|];
    [|(0.5f, -0.5f, 0.5f, 1.0f); (0.0f, 1.0f, 0.0f, 1.0f)|];
    [|(-0.5f, -0.5f, 0.5f, 1.0f); (0.0f, 0.0f, 1.0f, 1.0f)|]
|]

let myInputElements = [
    {Name = "POSITION"; Index = 0; Format = Format.R32G32B32A32_Float; Slot = 0; AlignedByteOffset = 0};  
    {Name = "COLOR"; Index = 0; Format = Format.R32G32B32A32_Float; Slot = 0; AlignedByteOffset = 16}
]
open assets
[<EntryPoint>]
let main argv = 

    let sphere = objAsset "../../nff/sphere.nff"

    let window = new RenderForm("drawlib!")
    let (device, swapChain) = deviceAndSwapChain window
    let (backbuffer, backbufferRtv) = prepareWindow window device swapChain
    let simpleVs, vsByteCode = vs device "MiniTri.fx"
    let simplePs, _ = ps device "MiniTri.fx"
    let toVec4 (x,y,z) = [|(x,y,z,1.f); (1.f,0.f,1.f,1.f)|]
    printfn "start making ib, vb"
    let ibVbs = sphere |> Array.map (fun x -> triVertexBuffer device (x.Vertices |> Array.map toVec4) x.Indices)
    printfn "end making ib, vb"
    let myLayout = layout device vsByteCode myInputElements 
    let immediateCtx = device.ImmediateContext
    

    RenderLoop.Run(window, (fun () ->                 
            let clearColor = new Color4(Color.Black.ToColor3())
            immediateCtx.ClearRenderTargetView(backbufferRtv, clearColor)
            for (ib, vb) in ibVbs do  
                        
                prepareInputAssembler immediateCtx myLayout vb ib
                prepareRasterizer immediateCtx (sizef window) 
                prepareOutputMerger immediateCtx backbufferRtv
                prepareShaders immediateCtx simpleVs simplePs

                immediateCtx.DrawIndexed(ib.IndicesCount, 0, 0)
            swapChain.Present(0, PresentFlags.None)            
        ))
        
    0 // return an integer exit code
