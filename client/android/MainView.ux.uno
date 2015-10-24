using Uno;
using Uno.Graphics;
using Uno.Platform;
using Uno.Collections;
using Fuse;
using Fuse.Controls;
using Fuse.Triggers;
using Fuse.Resources;
using Uno.Net.Sockets;

public partial class MainView
{
    Socket _socket;
    Buffer _buffer;

    void Connect(object a1, EventArgs a2)
    {

        if (_socket == null)
        {
            try
            {
                _buffer = new Buffer(4*5);
                _socket = Socket.Create(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
                _socket.Connect("127.0.0.1", 1234);
                SendAnnounceMessage(_socket, 1, "slider");
                ConnectButton.Text="Disconnect From Cepl";
            }
            catch (Exception e)
            {
                debug_log("Failed to connect: " + e.Message);
                _socket = null;
            }
        } else {
            try
            {
                _buffer = null;
                _socket.Close();
                _socket = null;
                ConnectButton.Text="Connect to Cepl";

            }
            catch (Exception e)
            {
                debug_log("Failed to disconnect: " + e.Message);
                _socket = null;
            }
        }
    }

    //--------------------------------------------------

    void Slid(object a1, EventArgs a2)
    {
        if (_socket==null) return;
        var slider = (Slider)a1;
        var normalizedVal = (float)(slider.Value/100.f);
        var sourceID = (uint)slider.GetHashCode();
        SendDataMessage(_socket, sourceID, float4(normalizedVal, 0.f, 0.f, 0.f));
    }

    void Panel2D(object a1, Fuse.Input.PointerMovedArgs a2)
    {
        if (_socket==null) return;
        var rect = (Rectangle)a1;
        var rectPos = float2(rect.WorldPosition.X, rect.WorldPosition.Y);
        var vec = float4(((a2.WindowPoint - rectPos) / rect.ActualSize), 0.f, 0.f);
        SendDataMessageFromUI(a1, vec);
    }

    void ButtonDown(object a1, Fuse.Input.PointerPressedArgs a2)
    {
        SendDataMessageFromUI(a1, float4(1.f, 0.f, 0.f, 0.f));
    }

    void ButtonUp(object a1, Fuse.Input.PointerReleasedArgs a2)
    {
        SendDataMessageFromUI(a1, float4(0.f, 0.f, 0.f, 0.f));
    }

    void SendDataMessageFromUI(object sender, float4 data)
    {
        if (_socket==null) return;
        var sourceID = (uint)sender.GetHashCode();
        SendDataMessage(_socket, sourceID, data);
    }

    //--------------------------------------------------

    static uint announce_source_id = 4294967295;
    static uint time_sync_id = 4294967294;

    void SendAnnounceMessage(Socket socket, uint sourceID, string name)
    {
        var bufferLen = 4 + 4 + 4 + (4 * name.Length);
        var buffer = new Buffer(bufferLen);

        buffer.Set(0, announce_source_id);
        buffer.Set(4, sourceID);
        buffer.Set(8, name.Length);

        for (var i = 0; i < name.Length; i++) {
            buffer.Set(12+(4*i), (int)name[i]);
        }

        socket.Send(extern<byte[]>(buffer) "@{Uno.Buffer:Of($0)._data}");
    }

    void SendDataMessage(Socket socket, uint sourceID, float4 vec)
    {
        _buffer.Set(0, sourceID);
        _buffer.Set(4, vec);
        socket.Send(extern<byte[]>(_buffer) "@{Uno.Buffer:Of($0)._data}");
    }
}

// - open socket in common lisp (e.g. 1237)
// - adb reverse tcp:1234 tcp:1237
// - the above will work
