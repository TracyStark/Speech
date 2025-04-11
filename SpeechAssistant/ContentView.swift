//
//  ContentView.swift
//  SpeechAssistant
//
//  Created by Xiaoyang Zhou on 4/5/25.
//

import SwiftUI
import Speech
import AVFoundation
import UniformTypeIdentifiers
import UIKit

// 句子状态枚举
enum SentenceState {
    case unread        // 未读
    case next         // 下一句
    case current      // 当前句
    case read         // 已读
    case extra        // 额外内容
}

// 句子模型
struct Sentence: Identifiable {
    let id = UUID()
    let text: String
    var state: SentenceState = .unread
    var extraContent: String = ""
}

// 添加录音文件模型
struct RecordingFile: Identifiable {
    let id = UUID()
    let url: URL
    let date: Date
    let duration: TimeInterval
    var customName: String?
    
    var formattedName: String {
        if let customName = customName {
            return customName
        }
        let dateFormatter = DateFormatter()
        dateFormatter.dateFormat = "yy/M/d HH:mm"
        return "\(dateFormatter.string(from: date)) \(Int(duration))s"
    }
}

// 在文件顶部添加新的结构体
struct SpeakerAnnotation {
    let id: Int
    let color: Color
    var name: String { "演讲者\(id)" }
}

class AnnotationManager: ObservableObject {
    @Published var speakers: [SpeakerAnnotation] = []
    @Published var annotations: [NSRange: Int] = [:]  // 存储文本范围和对应的演讲者ID
    
    func setSpeakerCount(_ count: Int) {
        speakers = (1...count).map { id in
            SpeakerAnnotation(id: id, color: getColorForSpeaker(id))
        }
    }
    
    private func getColorForSpeaker(_ id: Int) -> Color {
        let colors: [Color] = [
            .red.opacity(0.3),
            .blue.opacity(0.3),
            .green.opacity(0.3),
            .yellow.opacity(0.3),
            .purple.opacity(0.3),
            .orange.opacity(0.3),
            .pink.opacity(0.3),
            .cyan.opacity(0.3)
        ]
        return colors[(id - 1) % colors.count]
    }
    
    func annotateText(range: NSRange, speakerId: Int) {
        annotations[range] = speakerId
    }
    
    func clearAnnotation(range: NSRange) {
        annotations.removeValue(forKey: range)
    }
}

class SpeechRecognitionManager: ObservableObject {
    @Published var isRecording = false
    @Published var recognizedText = ""
    @Published var currentSentence = ""
    @Published var nextSentence = ""
    @Published var showPermissionAlert = false
    @Published var errorMessage = ""
    @Published var hasPermission = false
    @Published var sentences: [Sentence] = []
    @Published var currentIndex: Int = -1
    @Published var isPresentationMode = false
    
    private let speechRecognizer = SFSpeechRecognizer(locale: Locale(identifier: "zh-CN"))
    private var recognitionRequest: SFSpeechAudioBufferRecognitionRequest?
    private var recognitionTask: SFSpeechRecognitionTask?
    private let audioEngine = AVAudioEngine()
    
    private var lastRecognizedLength = 0
    private var recognitionBuffer = ""
    private var lastUpdateTime = Date()
    private var lastCompletionRate: Double = 0
    private let completionThreshold = 0.85  // 完成度阈值
    private let pauseThreshold = 0.3  // 停顿阈值
    private let stableThreshold = 0.1  // 稳定阈值
    private var consecutiveMatches = 0  // 连续匹配次数
    private let requiredConsecutiveMatches = 3  // 需要的连续匹配次数
    
    init() {
        checkPermissions()
    }
    
    func startPresentation(text: String) {
        // 重置状态
        lastRecognizedLength = 0
        recognitionBuffer = ""
        lastUpdateTime = Date()
        
        // 将文本分割成句子，保留标点符号
        var sentences: [String] = []
        var currentSentence = ""
        
        for char in text {
            currentSentence.append(char)
            if "。！？".contains(char) {
                if !currentSentence.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
                    sentences.append(currentSentence)
                }
                currentSentence = ""
            }
        }
        
        // 处理最后一个句子（如果没有以标点符号结尾）
        if !currentSentence.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
            sentences.append(currentSentence)
        }
        
        self.sentences = sentences.map { Sentence(text: $0.trimmingCharacters(in: .whitespacesAndNewlines)) }
        
        if !self.sentences.isEmpty {
            currentIndex = 0
            self.sentences[0].state = .current
            if self.sentences.count > 1 {
                self.sentences[1].state = .next
            }
        }
        
        isPresentationMode = true
        startRecording()
    }
    
    func stopPresentation() {
        stopRecording()
        isPresentationMode = false
        currentIndex = -1
        sentences = []
    }
    
    private func updateSentenceStates(recognizedText: String) {
        guard !sentences.isEmpty else { return }
        
        // 标准化识别文本：移除多余的空白字符和标点符号
        let recognizedWords = recognizedText.trimmingCharacters(in: .whitespacesAndNewlines)
            .replacingOccurrences(of: "\\s+", with: " ", options: .regularExpression)
            .replacingOccurrences(of: "[，。！？、]", with: "", options: .regularExpression)
        
        // 处理新增的文本
        if recognizedWords.count > lastRecognizedLength {
            let newText = String(recognizedWords.suffix(from: recognizedWords.index(recognizedWords.startIndex, offsetBy: lastRecognizedLength)))
            recognitionBuffer += newText
            lastRecognizedLength = recognizedWords.count
        }
        
        // 检查当前句子的完成度
        if let currentSentence = sentences.indices.contains(currentIndex) ? sentences[currentIndex] : nil {
            let currentText = currentSentence.text
                .trimmingCharacters(in: .whitespacesAndNewlines)
                .replacingOccurrences(of: "\\s+", with: " ", options: .regularExpression)
                .replacingOccurrences(of: "[，。！？、]", with: "", options: .regularExpression)
            
            // 标准化文本进行匹配
            let normalizedBuffer = recognitionBuffer
                .trimmingCharacters(in: .whitespacesAndNewlines)
                .replacingOccurrences(of: "\\s+", with: " ", options: .regularExpression)
                .replacingOccurrences(of: "[，。！？、]", with: "", options: .regularExpression)
            
            // 计算当前句子在识别文本中的匹配位置
            let similarity = calculateSimilarity(between: currentText, and: normalizedBuffer)
            
            // 更新完成度
            let completionRate = similarity
            
            // 检查完成度是否稳定或提高
            if completionRate >= lastCompletionRate {
                if completionRate > completionThreshold {
                    consecutiveMatches += 1
                }
            } else {
                consecutiveMatches = max(0, consecutiveMatches - 1)
            }
            
            // 提前预测即将完成，更新下一句的状态
            if completionRate > 0.5 && currentIndex + 1 < sentences.count {
                sentences[currentIndex + 1].state = .next
            }
            
            let now = Date()
            let timeSinceLastUpdate = now.timeIntervalSince(lastUpdateTime)
            
            // 满足以下条件时移动到下一句：
            // 1. 完成度超过阈值
            // 2. 达到足够的连续匹配次数
            // 3. 有一定的时间间隔
            if completionRate > completionThreshold && 
               consecutiveMatches >= requiredConsecutiveMatches && 
               timeSinceLastUpdate >= pauseThreshold {
                
                moveToNextSentence()
                
                // 清理识别缓冲区
                recognitionBuffer = normalizedBuffer
                
                lastUpdateTime = now
                consecutiveMatches = 0
            }
            
            lastCompletionRate = completionRate
        }
    }
    
    private func findBestMatch(target: String, in source: String) -> Range<Int>? {
        // 首先尝试直接包含匹配
        if source.contains(target) {
            if let range = source.range(of: target) {
                let start = source.distance(from: source.startIndex, to: range.lowerBound)
                let length = source.distance(from: range.lowerBound, to: range.upperBound)
                return start..<(start + length)
            }
        }
        
        // 如果直接匹配失败，尝试去除所有空白字符后再匹配
        let cleanTarget = target.replacingOccurrences(of: "\\s", with: "", options: .regularExpression)
        let cleanSource = source.replacingOccurrences(of: "\\s", with: "", options: .regularExpression)
        
        if cleanSource.contains(cleanTarget) {
            // 如果找到匹配，返回原始文本中对应的范围
            if let range = cleanSource.range(of: cleanTarget) {
                let start = cleanSource.distance(from: cleanSource.startIndex, to: range.lowerBound)
                let length = cleanSource.distance(from: range.lowerBound, to: range.upperBound)
                return start..<(start + length)
            }
        }
        
        return nil
    }
    
    private func moveToNextSentence() {
        guard currentIndex >= 0 && currentIndex < sentences.count else { return }
        
        // 标记当前句子为已读
        sentences[currentIndex].state = .read
        
        // 移动到下一句
        currentIndex += 1
        
        // 更新状态
        if currentIndex < sentences.count {
            // 设置当前句子状态
            sentences[currentIndex].state = .current
            
            // 预先设置下一句的状态
            if currentIndex + 1 < sentences.count {
                sentences[currentIndex + 1].state = .next
            }
            
            // 确保之前的句子都标记为已读
            for i in 0..<currentIndex {
                sentences[i].state = .read
            }
        }
    }
    
    // 优化相似度计算
    private func calculateSimilarity(between text1: String, and text2: String) -> Double {
        let chars1 = Array(text1.lowercased())
        let chars2 = Array(text2.lowercased())
        
        // 如果text2包含text1的大部分内容，提高相似度
        if text2.lowercased().contains(text1.lowercased()) {
            return 1.0
        }
        
        // 计算最长公共子序列
        let lcs = longestCommonSubsequence(chars1, chars2)
        var similarity = Double(lcs) / Double(chars1.count)
        
        // 如果识别文本包含原文的大部分内容，提高相似度
        if similarity > 0.7 {
            similarity = min(1.0, similarity + 0.2)
        }
        
        return similarity
    }
    
    // 计算最长公共子序列的长度
    private func longestCommonSubsequence(_ text1: [Character], _ text2: [Character]) -> Int {
        let m = text1.count
        let n = text2.count
        var dp = Array(repeating: Array(repeating: 0, count: n + 1), count: m + 1)
        
        for i in 1...m {
            for j in 1...n {
                if text1[i-1] == text2[j-1] {
                    dp[i][j] = dp[i-1][j-1] + 1
                } else {
                    dp[i][j] = max(dp[i-1][j], dp[i][j-1])
                }
            }
        }
        
        return dp[m][n]
    }
    
    private func findExtraContent(recognizedText: String, originalText: String) -> String? {
        // 移除原文内容，剩下的就是额外内容
        var remainingText = recognizedText
        
        // 移除所有已读句子的内容
        for i in 0...currentIndex {
            if i < sentences.count {
                remainingText = remainingText.replacingOccurrences(of: sentences[i].text, with: "")
            }
        }
        
        remainingText = remainingText.trimmingCharacters(in: .whitespacesAndNewlines)
        return remainingText.isEmpty ? nil : remainingText
    }
    
    func checkPermissions() {
        SFSpeechRecognizer.requestAuthorization { [weak self] authStatus in
            DispatchQueue.main.async {
                switch authStatus {
                case .authorized:
                    AVAudioApplication.requestRecordPermission { granted in
                        DispatchQueue.main.async {
                            if granted {
                                self?.hasPermission = true
                            } else {
                                self?.errorMessage = "需要麦克风权限才能进行录音"
                                self?.showPermissionAlert = true
                            }
                        }
                    }
                default:
                    self?.errorMessage = "需要语音识别权限才能使用此功能"
                    self?.showPermissionAlert = true
                }
            }
        }
    }
    
    func toggleRecording() {
        if isRecording {
            stopRecording()
        } else {
            startRecording()
        }
    }
    
    private func startRecording() {
        guard let speechRecognizer = speechRecognizer, speechRecognizer.isAvailable else {
            errorMessage = "语音识别器不可用"
            showPermissionAlert = true
            return
        }
        
        if !hasPermission {
            checkPermissions()
            return
        }
        
        do {
            if audioEngine.isRunning {
                audioEngine.stop()
                recognitionRequest?.endAudio()
                isRecording = false
                return
            }
            
            let audioSession = AVAudioSession.sharedInstance()
            try audioSession.setCategory(.record, mode: .measurement, options: .duckOthers)
            try audioSession.setActive(true, options: .notifyOthersOnDeactivation)
            
            recognitionRequest = SFSpeechAudioBufferRecognitionRequest()
            guard let recognitionRequest = recognitionRequest else {
                errorMessage = "无法创建语音识别请求"
                showPermissionAlert = true
                return
            }
            recognitionRequest.shouldReportPartialResults = true
            
            let inputNode = audioEngine.inputNode
            recognitionTask = speechRecognizer.recognitionTask(with: recognitionRequest) { [weak self] result, error in
                guard let self = self else { return }
                var isFinal = false
                
                if let result = result {
                    let recognizedText = result.bestTranscription.formattedString
                    DispatchQueue.main.async {
                        self.recognizedText = recognizedText
                        if self.isPresentationMode {
                            self.updateSentenceStates(recognizedText: recognizedText)
                        }
                    }
                    isFinal = result.isFinal
                }
                
                if error != nil || isFinal {
                    self.audioEngine.stop()
                    inputNode.removeTap(onBus: 0)
                    self.recognitionRequest = nil
                    self.recognitionTask = nil
                    self.isRecording = false
                }
            }
            
            let recordingFormat = inputNode.outputFormat(forBus: 0)
            inputNode.installTap(onBus: 0, bufferSize: 1024, format: recordingFormat) { [weak self] buffer, _ in
                self?.recognitionRequest?.append(buffer)
            }
            
            audioEngine.prepare()
            try audioEngine.start()
            isRecording = true
            
        } catch {
            errorMessage = "录音启动失败: \(error.localizedDescription)"
            showPermissionAlert = true
        }
    }
    
    private func stopRecording() {
        audioEngine.stop()
        recognitionRequest?.endAudio()
        audioEngine.inputNode.removeTap(onBus: 0)
        recognitionTask?.cancel()
        isRecording = false
    }
}

class AudioRecorder: NSObject, ObservableObject {
    @Published var recordings: [RecordingFile] = []
    @Published var isRecording = false
    @Published var audioLevel: Float = 0.0
    @Published var isPlaying = false
    @Published var currentlyPlayingId: UUID?
    
    private var audioRecorder: AVAudioRecorder?
    private var audioPlayer: AVAudioPlayer?
    private var levelTimer: Timer?
    private var recordingStartTime: Date?
    
    override init() {
        super.init()
    }
    
    func startRecording() {
        let audioFilename = getDocumentsDirectory().appendingPathComponent("\(Date().timeIntervalSince1970).m4a")
        let settings = [
            AVFormatIDKey: Int(kAudioFormatMPEG4AAC),
            AVSampleRateKey: 12000,
            AVNumberOfChannelsKey: 1,
            AVEncoderAudioQualityKey: AVAudioQuality.high.rawValue
        ]
        
        do {
            audioRecorder = try AVAudioRecorder(url: audioFilename, settings: settings)
            audioRecorder?.isMeteringEnabled = true
            audioRecorder?.record()
            isRecording = true
            recordingStartTime = Date()
            
            // 开始监测音量
            levelTimer = Timer.scheduledTimer(withTimeInterval: 0.1, repeats: true) { _ in
                self.audioRecorder?.updateMeters()
                self.audioLevel = self.audioRecorder?.averagePower(forChannel: 0) ?? -160
                self.audioLevel = (self.audioLevel + 160) / 160 // 归一化到0-1
            }
        } catch {
            print("录音失败: \(error)")
        }
    }
    
    func stopRecording() {
        audioRecorder?.stop()
        isRecording = false
        levelTimer?.invalidate()
        
        if let url = audioRecorder?.url, let startTime = recordingStartTime {
            let duration = Date().timeIntervalSince(startTime)
            let recording = RecordingFile(url: url, date: startTime, duration: duration)
            recordings.append(recording)
        }
        
        recordingStartTime = nil
    }
    
    func playRecording(_ recording: RecordingFile) {
        // 如果正在播放同一个录音，就停止播放
        if isPlaying && currentlyPlayingId == recording.id {
            stopPlayback()
            return
        }
        
        // 如果正在播放其他录音，先停止
        if isPlaying {
            stopPlayback()
        }
        
        do {
            // 设置音频会话
            let audioSession = AVAudioSession.sharedInstance()
            try audioSession.setCategory(.playback, mode: .default, options: [.mixWithOthers])
            try audioSession.setActive(true)
            
            // 创建并配置音频播放器
            audioPlayer = try AVAudioPlayer(contentsOf: recording.url)
            audioPlayer?.delegate = self
            audioPlayer?.prepareToPlay()  // 预加载音频
            audioPlayer?.volume = 1.0     // 设置最大音量
            audioPlayer?.play()
            
            isPlaying = true
            currentlyPlayingId = recording.id
        } catch {
            print("播放失败: \(error.localizedDescription)")
        }
    }
    
    func stopPlayback() {
        audioPlayer?.stop()
        isPlaying = false
        currentlyPlayingId = nil
    }
    
    private func getDocumentsDirectory() -> URL {
        FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    }
    
    func deleteRecording(_ recording: RecordingFile) {
        do {
            try FileManager.default.removeItem(at: recording.url)
            if let index = recordings.firstIndex(where: { $0.id == recording.id }) {
                recordings.remove(at: index)
            }
        } catch {
            print("删除录音失败: \(error)")
        }
    }
    
    func renameRecording(_ recording: RecordingFile, newName: String) {
        if let index = recordings.firstIndex(where: { $0.id == recording.id }) {
            var updatedRecording = recording
            updatedRecording.customName = newName
            recordings[index] = updatedRecording
        }
    }
}

// 添加 AVAudioPlayerDelegate 扩展
extension AudioRecorder: AVAudioPlayerDelegate {
    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        DispatchQueue.main.async {
            self.isPlaying = false
            self.currentlyPlayingId = nil
        }
    }
}

struct RecordingItemView: View {
    let recording: RecordingFile
    let isPlaying: Bool
    let onTap: () -> Void
    let onDelete: () -> Void
    let onRename: (String) -> Void
    
    @State private var isShowingRenameDialog = false
    @State private var isShowingDeleteDialog = false
    @State private var newName = ""

    var body: some View {
        HStack {
            Button(action: onTap) {
                HStack {
                    Image(systemName: isPlaying ? "stop.circle.fill" : "play.circle.fill")
                        .foregroundColor(isPlaying ? .red : .black)
                    Text(recording.formattedName)
                    Spacer()
                }
            }
            .buttonStyle(PlainButtonStyle())
            
            Menu {
                Button(action: { 
                    isShowingRenameDialog = true
                    newName = recording.customName ?? recording.formattedName
                }) {
                    Label("重命名", systemImage: "pencil")
                }
                
                Button(role: .destructive, action: { isShowingDeleteDialog = true }) {
                    Label("删除", systemImage: "trash")
                }
                    } label: {
                Image(systemName: "ellipsis.circle")
                    .foregroundColor(.gray)
            }
        }
        .alert("重命名录音", isPresented: $isShowingRenameDialog) {
            TextField("新名称", text: $newName)
            Button("取消", role: .cancel) { }
            Button("确定") {
                onRename(newName)
            }
        } message: {
            Text("请输入新的名称")
        }
        .alert("删除录音", isPresented: $isShowingDeleteDialog) {
            Button("取消", role: .cancel) { }
            Button("删除", role: .destructive) {
                onDelete()
            }
        } message: {
            Text("确定要删除这个录音吗？此操作无法撤销。")
        }
    }
}

struct AudioLevelView: View {
    let level: Float

    var body: some View {
        GeometryReader { geometry in
            ZStack(alignment: .leading) {
                Rectangle()
                    .fill(Color.gray.opacity(0.3))
                Rectangle()
                    .fill(Color.black)
                    .frame(width: CGFloat(level) * geometry.size.width)
            }
        }
        .frame(height: 4)
        .cornerRadius(2)
    }
}

struct TimerView: View {
    let elapsedTime: TimeInterval
    
    var formattedTime: String {
        let minutes = Int(elapsedTime) / 60
        let seconds = Int(elapsedTime) % 60
        return String(format: "%02d:%02d", minutes, seconds)
    }

    var body: some View {
        Text(formattedTime)
            .font(.system(.title2, design: .monospaced))
            .padding(8)
            .background(Color.black)
            .foregroundColor(.white)
            .cornerRadius(8)
    }
}

struct CheckboxStyle: ToggleStyle {
    func makeBody(configuration: Configuration) -> some View {
        HStack {
            configuration.label
            Spacer()
            Image(systemName: configuration.isOn ? "checkmark.square.fill" : "square")
                .foregroundColor(configuration.isOn ? .black : .gray)
                .onTapGesture {
                    configuration.isOn.toggle()
                }
        }
    }
}

struct PresentationTextView: View {
    let sentences: [Sentence]
    let fontSize: CGFloat

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 10) {
                ForEach(sentences) { sentence in
                    HStack(spacing: 4) {
                        Text(sentence.text)
                            .font(.system(size: fontSize))
                            .padding(8)
                            .background(backgroundColor(for: sentence.state))
                            .cornerRadius(4)
                        
                        if !sentence.extraContent.isEmpty {
                            Image(systemName: "circle.fill")
                                .foregroundColor(.red)
                                .font(.system(size: 8))
                        }
                    }
                }
            }
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding()
        }
    }
    
    private func backgroundColor(for state: SentenceState) -> Color {
        switch state {
        case .unread:
            return .clear
        case .next:
            return .blue.opacity(0.2)
        case .current:
            return .yellow.opacity(0.3)
        case .read:
            return .gray.opacity(0.2)
        case .extra:
            return .gray.opacity(0.2)
        }
    }
}

struct AnnotatedText: View {
    let text: String
    let annotations: [NSRange: Int]
    let speakers: [SpeakerAnnotation]
    
    var body: some View {
        Text(text)
            .background(
                GeometryReader { geometry in
                    ZStack(alignment: .topLeading) {
                        ForEach(Array(annotations.keys), id: \.self) { range in
                            if let speakerId = annotations[range],
                               let speaker = speakers.first(where: { $0.id == speakerId }) {
                                let start = text.index(text.startIndex, offsetBy: range.location)
                                let end = text.index(start, offsetBy: range.length)
                                let substring = text[start..<end]
                                
                                Text(String(substring))
                                    .background(speaker.color)
                                    .position(x: geometry.size.width * 0.5, y: geometry.size.height * 0.5)
                            }
                        }
                    }
                }
            )
    }
}

struct TextEditorView: View {
    @Binding var text: String
    @ObservedObject var annotationManager: AnnotationManager
    @Binding var isEditMode: Bool
    @ObservedObject var scriptManager: SpeechScriptManager
    @Binding var scriptTitle: String
    let fontSize: CGFloat
    @State private var selectedRange: NSRange?
    @State private var editingText: String = ""
    
    var body: some View {
        VStack(spacing: 10) {
            // 编辑模式下的保存和取消按钮
            HStack {
                Spacer()
                Button(action: {
                    isEditMode = false
                }) {
                    Text("取消")
                        .foregroundColor(.white)
                        .padding(.horizontal, 20)
                        .padding(.vertical, 10)
                        .background(Color.black)
                        .cornerRadius(8)
                }
                
                Button(action: {
                    // 创建新的演讲稿
                    let script = SpeechScript(
                        title: scriptTitle,
                        content: editingText,
                        isFromFile: false
                    )
                    scriptManager.addScript(script)
                    text = editingText
                    isEditMode = false
                }) {
                    Text("保存")
                        .foregroundColor(.white)
                        .padding(.horizontal, 20)
                        .padding(.vertical, 10)
                        .background(Color.black)
                        .cornerRadius(8)
                }
            }
            .padding(.horizontal)
            
            CustomTextView(text: $editingText,
                          fontSize: fontSize,
                          selectedRange: $selectedRange,
                          annotationManager: annotationManager,
                          isEditable: true)  // 在编辑模式下设置为可编辑
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .padding(4)
                .background(Color.white)
                .cornerRadius(8)
                .overlay(
                    RoundedRectangle(cornerRadius: 8)
                        .stroke(Color.gray.opacity(0.3), lineWidth: 1)
                )
        }
        .onAppear {
            editingText = text
        }
    }
}

struct CustomTextView: UIViewRepresentable {
    @Binding var text: String
    let fontSize: CGFloat
    @Binding var selectedRange: NSRange?
    @ObservedObject var annotationManager: AnnotationManager
    let isEditable: Bool  // 添加是否可编辑的属性
    
    func makeUIView(context: Context) -> UITextView {
        let textView = UITextView()
        textView.delegate = context.coordinator
        textView.isEditable = isEditable  // 设置是否可编辑
        textView.isSelectable = true      // 保持可选择
        textView.font = .systemFont(ofSize: fontSize)
        return textView
    }
    
    func updateUIView(_ uiView: UITextView, context: Context) {
        // 更新字体大小
        uiView.font = .systemFont(ofSize: fontSize)
        
        // 更新文本属性
        let attributedString = NSMutableAttributedString(string: text)
        
        // 设置基础字体属性
        attributedString.addAttribute(.font, 
                                    value: UIFont.systemFont(ofSize: fontSize), 
                                    range: NSRange(location: 0, length: text.count))
        
        // 添加标注颜色
        for (range, speakerId) in annotationManager.annotations {
            if let speaker = annotationManager.speakers.first(where: { $0.id == speakerId }) {
                attributedString.addAttribute(.backgroundColor, 
                                            value: UIColor(speaker.color), 
                                            range: range)
            }
        }
        
        // 只有当文本内容改变时才更新
        if uiView.attributedText.string != text {
            uiView.attributedText = attributedString
        }
    }
    
    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }
    
    class Coordinator: NSObject, UITextViewDelegate {
        var parent: CustomTextView
        
        init(_ parent: CustomTextView) {
            self.parent = parent
        }
        
        func textViewDidChange(_ textView: UITextView) {
            parent.text = textView.text
        }
        
        func textViewDidChangeSelection(_ textView: UITextView) {
            if let selectedRange = textView.selectedTextRange {
                let start = textView.offset(from: textView.beginningOfDocument, to: selectedRange.start)
                let end = textView.offset(from: textView.beginningOfDocument, to: selectedRange.end)
                parent.selectedRange = NSRange(location: start, length: end - start)
            }
        }
    }
}

struct SpeakerColorPicker: View {
    @ObservedObject var annotationManager: AnnotationManager
    @Binding var selectedSpeakerId: Int?
    
    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 15) {
                ForEach(annotationManager.speakers, id: \.id) { speaker in
                    Button(action: {
                        selectedSpeakerId = speaker.id
                    }) {
                        VStack {
                            Circle()
                                .fill(speaker.color)
                                .frame(width: 30, height: 30)
                                .overlay(
                                    Circle()
                                        .stroke(selectedSpeakerId == speaker.id ? Color.black : Color.clear, lineWidth: 2)
                                )
                            Text(speaker.name)
                                .font(.caption)
                        }
                    }
                }
            }
            .padding(.horizontal)
        }
        .frame(height: 60)
        .background(Color.gray.opacity(0.1))
    }
}

struct TextDisplayView: View {
    @Binding var text: String
    @Binding var isEditMode: Bool
    @ObservedObject var speechManager: SpeechRecognitionManager
    @ObservedObject var annotationManager: AnnotationManager
    let fontSize: CGFloat
    @State private var editingText: String = ""
    
    var body: some View {
        ScrollView {
            VStack {
                if isEditMode {
                    TextEditorView(text: $text, 
                                 annotationManager: annotationManager, 
                                 isEditMode: $isEditMode,
                                 scriptManager: SpeechScriptManager(),
                                 scriptTitle: $editingText,
                                 fontSize: fontSize)
                } else if speechManager.isPresentationMode {
                    PresentationTextView(sentences: speechManager.sentences, fontSize: fontSize)
                } else {
                    Text(text.isEmpty ? "请选择或输入演讲稿" : text)
                        .font(.system(size: fontSize))
                        .frame(maxWidth: .infinity, alignment: .leading)
                        .padding()
                }
            }
            .frame(maxWidth: .infinity, minHeight: UIScreen.main.bounds.height - 100)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color.gray.opacity(0.1))
        .cornerRadius(15)
        .onAppear {
            editingText = text
        }
    }
}

// 演讲稿模型
struct SpeechScript: Identifiable, Codable {
    let id: UUID
    var title: String
    var content: String
    let createdDate: Date
    let isFromFile: Bool
    let sourceFileName: String?
    
    init(id: UUID = UUID(), title: String, content: String, isFromFile: Bool = false, sourceFileName: String? = nil) {
        self.id = id
        self.title = title
        self.content = content
        self.createdDate = Date()
        self.isFromFile = isFromFile
        self.sourceFileName = sourceFileName
    }
}

// 演讲稿管理器
class SpeechScriptManager: ObservableObject {
    @Published var scripts: [SpeechScript] = []
    
    // 从 UserDefaults 加载保存的演讲稿
    init() {
        if let data = UserDefaults.standard.data(forKey: "savedScripts"),
           let decoded = try? JSONDecoder().decode([SpeechScript].self, from: data) {
            scripts = decoded
        }
    }
    
    // 保存演讲稿到 UserDefaults
    private func saveScripts() {
        if let encoded = try? JSONEncoder().encode(scripts) {
            UserDefaults.standard.set(encoded, forKey: "savedScripts")
        }
    }
    
    // 添加新的演讲稿
    func addScript(_ script: SpeechScript) {
        scripts.append(script)
        saveScripts()
    }
    
    // 删除演讲稿
    func deleteScript(_ script: SpeechScript) {
        scripts.removeAll { $0.id == script.id }
        saveScripts()
    }
}

// 选择演讲稿视图
struct SpeechScriptSelectionView: View {
    @Environment(\.dismiss) private var dismiss
    @ObservedObject var scriptManager: SpeechScriptManager
    @Binding var selectedScript: String
    @State private var isShowingFilePicker = false
    @State private var showErrorAlert = false
    @State private var errorMessage = ""
    
    // 支持的文件类型
    private let supportedFileTypes: [UTType] = [
        .text,
        .plainText,
        .rtf
    ]
    
    var body: some View {
        HStack(spacing: 0) {
            // 左侧预览区域
            VStack {
                Text("演讲稿预览")
                    .font(.title)
                    .padding()
                
                if let script = scriptManager.scripts.first {
                    Text(script.content)
                        .padding()
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                } else {
                    Text("暂无演讲稿预览")
                        .foregroundColor(.gray)
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                }
            }
            .frame(maxWidth: .infinity)
            .background(Color(.systemGray6))
            
            // 右侧控制区域
            VStack(spacing: 20) {
                // 导入新演讲稿按钮
                Button(action: {
                    isShowingFilePicker = true
                }) {
                    Text("导入新的演讲稿")
                        .font(.headline)
                        .foregroundColor(.white)
                        .frame(maxWidth: .infinity)
                        .padding()
                        .background(Color.black)
                        .cornerRadius(10)
                }
                .padding(.horizontal)
                
                // 演讲稿列表
                Text("演讲稿列表")
                    .font(.title2)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .padding(.horizontal)
                
                List {
                    ForEach(scriptManager.scripts) { script in
                        VStack(alignment: .leading) {
                            Text(script.title)
                                .font(.headline)
                            Text(script.isFromFile ? "来自文件：\(script.sourceFileName ?? "")" : "手动创建")
                                .font(.caption)
                                .foregroundColor(.gray)
                        }
                        .contentShape(Rectangle())
                        .onTapGesture {
                            selectedScript = script.content
                            dismiss()
                        }
                    }
                    .onDelete { indexSet in
                        indexSet.forEach { index in
                            scriptManager.deleteScript(scriptManager.scripts[index])
                        }
                    }
                }
                
                // 准备演讲按钮
                Button(action: {
                    dismiss()
                }) {
                    HStack {
                        Text("准备演讲")
                            .font(.headline)
                        Image(systemName: "arrow.right.circle.fill")
                    }
                    .foregroundColor(.white)
                    .frame(maxWidth: .infinity)
                    .padding()
                    .background(Color.black)
                    .cornerRadius(10)
                }
                .padding(.horizontal)
            }
            .frame(width: 300)
            .padding(.vertical)
        }
        .fileImporter(
            isPresented: $isShowingFilePicker,
            allowedContentTypes: supportedFileTypes,
            allowsMultipleSelection: false
        ) { result in
            switch result {
            case .success(let urls):
                guard let url = urls.first else { return }
                do {
                    guard url.startAccessingSecurityScopedResource() else {
                        errorMessage = "无法访问文件"
                        showErrorAlert = true
                        return
                    }
                    defer { url.stopAccessingSecurityScopedResource() }
                    
                    let content = try String(contentsOf: url, encoding: .utf8)
                    let fileName = url.lastPathComponent
                    let script = SpeechScript(
                        title: fileName,
                        content: content,
                        isFromFile: true,
                        sourceFileName: fileName
                    )
                    scriptManager.addScript(script)
                    
                } catch {
                    errorMessage = "读取文件失败: \(error.localizedDescription)"
                    showErrorAlert = true
                }
            case .failure(let error):
                errorMessage = "选择文件失败: \(error.localizedDescription)"
                showErrorAlert = true
            }
        }
        .alert("错误", isPresented: $showErrorAlert) {
            Button("确定", role: .cancel) { }
        } message: {
            Text(errorMessage)
        }
    }
}

// 添加标注视图
struct AnnotationView: View {
    @Environment(\.dismiss) private var dismiss
    @Binding var text: String
    @ObservedObject var annotationManager: AnnotationManager
    @State private var selectedRange: NSRange?
    @State private var selectedSpeakerId: Int?
    @State private var isShowingSpeakerDialog = false
    @State private var speakerCount = "2"
    
    var body: some View {
        VStack(spacing: 10) {
            // 顶部按钮
            HStack {
                Spacer()
                Button(action: {
                    dismiss()
                }) {
                    Text("取消")
                        .foregroundColor(.white)
                        .padding(.horizontal, 20)
                        .padding(.vertical, 10)
                        .background(Color.black)
                        .cornerRadius(8)
                }
                
                Button(action: {
                    // 保存标注
                    dismiss()
                }) {
                    Text("保存")
                        .foregroundColor(.white)
                        .padding(.horizontal, 20)
                        .padding(.vertical, 10)
                        .background(Color.black)
                        .cornerRadius(8)
                }
            }
            .padding(.horizontal)
            
            // 设置演讲者和颜色选择器
            VStack(alignment: .leading, spacing: 10) {
                Button("设置演讲者") {
                    isShowingSpeakerDialog = true
                }
                .buttonStyle(.bordered)
                
                if !annotationManager.speakers.isEmpty {
                    SpeakerColorPicker(
                        annotationManager: annotationManager,
                        selectedSpeakerId: $selectedSpeakerId
                    )
                }
            }
            .padding()
            .background(Color.gray.opacity(0.1))
            .cornerRadius(10)
            
            // 只读的文本视图，支持选择
            CustomTextView(text: .constant(text),  // 使用 constant 使其只读
                         fontSize: 18,
                         selectedRange: $selectedRange,
                         annotationManager: annotationManager,
                         isEditable: false)  // 设置为不可编辑
                .frame(maxWidth: .infinity, maxHeight: .infinity)
                .padding(4)
                .background(Color.white)
                .cornerRadius(8)
                .overlay(
                    RoundedRectangle(cornerRadius: 8)
                        .stroke(Color.gray.opacity(0.3), lineWidth: 1)
                )
        }
        .onChange(of: selectedRange) { newRange in
            if let range = newRange, let speakerId = selectedSpeakerId {
                // 如果已选择演讲者和文本范围，添加标注
                annotationManager.annotateText(range: range, speakerId: speakerId)
            }
        }
        .onChange(of: selectedSpeakerId) { newSpeakerId in
            if let range = selectedRange, let speakerId = newSpeakerId {
                // 如果已选择文本范围和演讲者，添加标注
                annotationManager.annotateText(range: range, speakerId: speakerId)
            }
        }
        .alert("设置演讲者", isPresented: $isShowingSpeakerDialog) {
            TextField("演讲者人数", text: $speakerCount)
                .keyboardType(.numberPad)
            Button("取消", role: .cancel) { }
            Button("确定") {
                if let count = Int(speakerCount), count > 0 {
                    annotationManager.setSpeakerCount(count)
                }
            }
        } message: {
            Text("请输入演讲者人数")
        }
    }
}

struct ContentView: View {
    @StateObject private var speechManager = SpeechRecognitionManager()
    @StateObject private var audioRecorder = AudioRecorder()
    @State private var fontSize: CGFloat = 18
    @State private var isRecordingEnabled = false
    @State private var isTimerEnabled = false
    @State private var isEditMode = false
    @State private var isAnnotationMode = false
    @State private var speechText = ""
    @State private var showConfirmAlert = false
    @State private var elapsedTime: TimeInterval = 0
    @State private var timer: Timer?
    @StateObject private var annotationManager = AnnotationManager()
    @State private var selectedSpeakerId: Int?
    @State private var isShowingSpeakerDialog = false
    @State private var speakerCount = "2"
    @State private var isShowingScriptSelection = false
    @State private var showErrorAlert = false
    @StateObject private var scriptManager = SpeechScriptManager()
    @State private var errorMessage = ""  // 添加错误消息变量
    @State private var scriptTitle: String = "未命名演讲稿1"  // 添加标题状态
    @State private var isShowingAnnotationView = false  // 添加标注视图的状态
    
    // 支持的文件类型
    private let supportedFileTypes: [UTType] = [
        .text,
        .plainText,
        .rtf,
    ]
    
    var body: some View {
        HStack(spacing: 20) {
            // 左侧文本显示区域
            TextDisplayView(text: $speechText, 
                          isEditMode: $isEditMode, 
                          speechManager: speechManager,
                          annotationManager: annotationManager,
                          fontSize: fontSize)
            
            // 右侧控制面板
            VStack(spacing: 15) {
                // 开始演讲按钮
                Button(action: {
                    if isEditMode {
                        showConfirmAlert = true
                    } else if speechManager.isPresentationMode {
                        speechManager.stopPresentation()
                        if isRecordingEnabled {
                            audioRecorder.stopRecording()
                        }
                        if isTimerEnabled {
                            timer?.invalidate()
                            timer = nil
                        }
                    } else {
                        speechManager.startPresentation(text: speechText)
                        if isRecordingEnabled {
                            audioRecorder.startRecording()
                        }
                        if isTimerEnabled {
                            elapsedTime = 0
                            timer = Timer.scheduledTimer(withTimeInterval: 1, repeats: true) { _ in
                                elapsedTime += 1
                            }
                        }
                    }
                }) {
                    HStack {
                        Text(speechManager.isPresentationMode ? "结束演讲" : "开始演讲")
                            .font(.headline)
                        Image(systemName: speechManager.isPresentationMode ? "stop.circle.fill" : "arrow.right.circle.fill")
                    }
                    .foregroundColor(.white)
                    .frame(maxWidth: .infinity)
                    .padding()
                    .background(isEditMode || speechText.isEmpty ? Color.gray : Color.black)
                    .cornerRadius(10)
                }
                .disabled(isEditMode || speechText.isEmpty)
                
                // 录音和计时选项
                VStack(spacing: 10) {
                    Toggle(isOn: $isRecordingEnabled) {
                        Text("录音")
                    }
                    .toggleStyle(CheckboxStyle())
                    .disabled(speechManager.isPresentationMode || isEditMode)
                    
                    Toggle(isOn: $isTimerEnabled) {
                        Text("计时")
                    }
                    .toggleStyle(CheckboxStyle())
                    .disabled(speechManager.isPresentationMode || isEditMode)
                }
                .padding(.vertical, 5)
                
                // 显示录音音量和计时器（仅在演讲模式下显示）
                if speechManager.isPresentationMode {
                    if isRecordingEnabled {
                        VStack(alignment: .leading, spacing: 5) {
                            Text("录音音量")
                                .font(.caption)
                            AudioLevelView(level: audioRecorder.audioLevel)
                        }
                        .padding(.vertical, 5)
                    }
                    
                    if isTimerEnabled {
                        TimerView(elapsedTime: elapsedTime)
                    }
                }
                
                // 字体大小调节
                HStack {
                    Text("字体大小")
                    Spacer()
                    Button(action: { fontSize -= 2 }) {
                        Image(systemName: "minus.circle.fill")
                            .foregroundColor(speechManager.isPresentationMode ? .gray : .black)
                    }
                    .disabled(speechManager.isPresentationMode)
                    Text("\(Int(fontSize))")
                        .frame(width: 40)
                    Button(action: { fontSize += 2 }) {
                        Image(systemName: "plus.circle.fill")
                            .foregroundColor(speechManager.isPresentationMode ? .gray : .black)
                    }
                    .disabled(speechManager.isPresentationMode)
                }
                
                // 编辑和标注按钮
                HStack(spacing: 10) {
                    Button(action: { 
                        if !isEditMode {
                            isEditMode = true
                            isAnnotationMode = false
                        }
                    }) {
                        Text("编辑")
                            .frame(maxWidth: .infinity)
                            .padding()
                            .background(isEditMode ? Color.gray : (speechManager.isPresentationMode ? Color.gray : Color.black))
                            .foregroundColor(.white)
                            .cornerRadius(8)
                    }
                    .disabled(speechManager.isPresentationMode || isEditMode)
                    
                    Button(action: { 
                        isShowingAnnotationView = true
                    }) {
                        Text("标注")
                            .frame(maxWidth: .infinity)
                            .padding()
                            .background(speechManager.isPresentationMode || isEditMode ? Color.gray : Color.black)
                            .foregroundColor(.white)
                            .cornerRadius(8)
                    }
                    .disabled(speechManager.isPresentationMode || isEditMode)
                }
                
                // 选择演讲稿按钮
                Button(action: {
                    if isEditMode {
                        showConfirmAlert = true
                    } else {
                        isShowingScriptSelection = true
                    }
                }) {
                    HStack {
                        Text("选择演讲稿")
                            .font(.headline)
                        Image(systemName: "arrow.right.circle.fill")
                    }
                    .foregroundColor(.white)
                    .frame(maxWidth: .infinity)
                    .padding()
                    .background(speechManager.isPresentationMode || isEditMode ? Color.gray : Color.black)
                    .cornerRadius(10)
                }
                .disabled(speechManager.isPresentationMode || isEditMode)
                .fullScreenCover(isPresented: $isShowingScriptSelection) {
                    SpeechScriptSelectionView(scriptManager: scriptManager, selectedScript: $speechText)
                }
                
                // 实时语音识别内容显示框
                VStack(alignment: .leading) {
                    Text("实时语音识别")
                        .font(.headline)
                    ScrollView {
                        Text(speechManager.recognizedText)
                            .font(.system(size: 14))
                            .frame(maxWidth: .infinity, alignment: .leading)
                            .padding(8)
                    }
                    .frame(height: 100)
                    .background(Color.gray.opacity(0.1))
                    .cornerRadius(10)
                }
                
                // 在编辑模式下显示标题输入框
                if isEditMode {
                    VStack(alignment: .leading, spacing: 5) {
                        Text("演讲稿标题")
                            .font(.headline)
                        TextField("未命名演讲稿", text: $scriptTitle)
                            .textFieldStyle(RoundedBorderTextFieldStyle())
                            .onAppear {
                                // 只在首次显示时设置默认标题
                                if scriptTitle == "未命名演讲稿1" {
                                    let existingUntitledCount = scriptManager.scripts.filter { $0.title.hasPrefix("未命名演讲稿") }.count
                                    if existingUntitledCount > 0 {
                                        scriptTitle = "未命名演讲稿\(existingUntitledCount + 1)"
                                    }
                                }
                            }
                    }
                    .padding(.vertical, 5)
                }
                
                // 历史录音列表
                if !audioRecorder.recordings.isEmpty {
                    VStack(alignment: .leading) {
                        Text("历史录音")
                            .font(.headline)
                        ScrollView {
                            VStack(alignment: .leading, spacing: 8) {
                                ForEach(audioRecorder.recordings) { recording in
                                    RecordingItemView(
                                        recording: recording,
                                        isPlaying: audioRecorder.isPlaying && audioRecorder.currentlyPlayingId == recording.id,
                                        onTap: { audioRecorder.playRecording(recording) },
                                        onDelete: { audioRecorder.deleteRecording(recording) },
                                        onRename: { newName in 
                                            audioRecorder.renameRecording(recording, newName: newName)
                                        }
                                    )
                                }
                            }
                            .padding(8)
                        }
                        .frame(height: 100)
                        .background(Color.gray.opacity(0.1))
                        .cornerRadius(10)
                    }
                }
                
                Spacer()
            }
            .frame(width: 250)
            .padding()
        }
        .padding()
        .alert("错误", isPresented: $speechManager.showPermissionAlert) {
            Button("确定", role: .cancel) { }
        } message: {
            Text(speechManager.errorMessage)
        }
        .alert("正在编辑", isPresented: $showConfirmAlert) {
            Button("继续编辑", role: .cancel) { }
            Button("放弃编辑", role: .destructive) {
                isEditMode = false
            }
        } message: {
            Text("您有未保存的编辑内容，是否要放弃编辑？")
        }
        .alert("设置演讲者", isPresented: $isShowingSpeakerDialog) {
            TextField("演讲者人数", text: $speakerCount)
                .keyboardType(.numberPad)
            Button("取消", role: .cancel) { }
            Button("确定") {
                if let count = Int(speakerCount), count > 0 {
                    annotationManager.setSpeakerCount(count)
                }
            }
        } message: {
            Text("请输入演讲者人数")
        }
        .alert("错误", isPresented: $showErrorAlert) {
            Button("确定", role: .cancel) { }
        } message: {
            Text(errorMessage)
        }
        .fullScreenCover(isPresented: $isShowingAnnotationView) {
            AnnotationView(text: $speechText, annotationManager: annotationManager)
        }
    }
    
    // 处理文本内容，添加分段
    private func processTextContent(_ content: String) -> String {
        // 按段落分割
        let paragraphs = content.components(separatedBy: .newlines)
        
        // 处理每个段落
        var processedParagraphs: [String] = []
        for paragraph in paragraphs {
            let trimmed = paragraph.trimmingCharacters(in: .whitespacesAndNewlines)
            if !trimmed.isEmpty {
                // 如果段落以标点符号结尾，直接添加
                if "。！？".contains(trimmed.last ?? Character("")) {
                    processedParagraphs.append(trimmed)
                } else {
                    // 否则添加句号
                    processedParagraphs.append(trimmed + "。")
                }
            }
        }
        
        // 用换行符连接处理后的段落
        return processedParagraphs.joined(separator: "\n")
    }
}

#Preview {
    ContentView()
}

