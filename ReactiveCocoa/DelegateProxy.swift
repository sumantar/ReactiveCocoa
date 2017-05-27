import ReactiveSwift
import enum Result.NoError

private let delegateProxySetupKey = AssociationKey(default: false)
private let hasSwizzledKey = AssociationKey<Bool>(default: false)

public protocol DelegateProxyProtocol: class {}

internal protocol _DelegateProxyProtocol: class {
	func proxyWillIntercept(_ selector: Selector)
	var lifetime: Lifetime { get }
}

public final class DelegateProxy<Delegate: NSObjectProtocol>: NSObject, DelegateProxyProtocol, _DelegateProxyProtocol {
	public var delegateType: Delegate.Type {
		return Delegate.self
	}

	public weak var forwardee: Delegate? {
		didSet {
			originalSetter(self)
		}
	}

	internal let lifetime: Lifetime

	private let writeLock = NSLock()
	private var interceptedSelectors: Set<Selector> = []
	private let originalSetter: (AnyObject) -> Void
	private let objcProtocol: Protocol

	fileprivate init(objcProtocol: Protocol, lifetime: Lifetime, originalSetter: @escaping (AnyObject) -> Void) {
		DelegateProxy<Delegate>.implementRequiredMethods(in: objcProtocol)

		self.objcProtocol = objcProtocol
		self.lifetime = lifetime
		self.originalSetter = originalSetter

		super.init()
	}

	public override func forwardingTarget(for selector: Selector!) -> Any? {
		// This is a fast path for selectors that are not being intercepted.
		return interceptedSelectors.contains(selector) ? nil : forwardee
	}

	public override func responds(to selector: Selector!) -> Bool {
		if interceptedSelectors.contains(selector) {
			return true
		}

		return (forwardee?.responds(to: selector) ?? false) || super.responds(to: selector)
	}

	// # Implementation Note
	//
	// Unlike method interception, the specialized DelegateProxy itself is being swizzled
	// in `proxyWillIntercept` and `implementRequiredMethods(in:)`, not the runtime
	// subclass.

	internal func proxyWillIntercept(_ selector: Selector) {
		let subclass: AnyClass = swizzleClass(self)
		let perceivedClass: AnyClass = (DelegateProxy<Delegate>.self as AnyObject).objcClass

		try! ReactiveCocoa.synchronized(subclass) {
			if class_getImmediateMethod(perceivedClass, selector) == nil {
				// All required methods should have been implemented. So we only implement
				// optional methods dynamically.
				let description = protocol_getMethodDescription(objcProtocol, selector, false, true)

				guard let typeEncoding = description.types else {
					fatalError("The selector `\(String(describing: selector))` does not exist.")
				}

				precondition(typeEncoding.pointee == Int8(UInt8(ascii: "v")),
							 "DelegateProxy does not support intercepting methods that returns non-void types.")

				let replacedImpl = class_replaceMethod(perceivedClass, selector, _rac_objc_msgForward, typeEncoding)

				precondition(replacedImpl == nil || replacedImpl == _rac_objc_msgForward,
							 "Swizzling DelegateProxy with other libraries is not supported.")
			}
		}

		writeLock.lock()
		interceptedSelectors.insert(selector)
		writeLock.unlock()

		originalSetter(self)
	}

	fileprivate static func implementRequiredMethods(in objcProtocol: Protocol) {
		let perceivedClass: AnyClass = (self as AnyObject).objcClass
		let classAssociations = Associations(perceivedClass as AnyObject)

		try! ReactiveCocoa.synchronized(perceivedClass) {
			guard !classAssociations.value(forKey: delegateProxySetupKey) else {
				return
			}

			var count: UInt32 = 0
			if let list = protocol_copyMethodDescriptionList(objcProtocol, true, true, &count) {
				let buffer = UnsafeBufferPointer(start: list, count: Int(count))
				defer { free(list) }

				for method in buffer {
					precondition(method.types.pointee == Int8(UInt8(ascii: "v")),
					             "DelegateProxy does not support protocols with required methods that returns non-void types.")

					_ = class_addMethod(perceivedClass, method.name, _rac_objc_msgForward, method.types)
				}
			}

			let _forwardInvocation: @convention(block) (Unmanaged<AnyObject>, Unmanaged<AnyObject>) -> Void = { object, invocation in
				if let forwardee = (object.takeUnretainedValue() as! DelegateProxy<Delegate>).forwardee {
					let invocation = invocation.takeUnretainedValue()

					if forwardee.responds(to: invocation.selector) {
						invocation.invoke(withTarget: forwardee)
					}
				}
			}

			let previousImpl = class_replaceMethod(perceivedClass,
			                                       ObjCSelector.forwardInvocation,
			                                       imp_implementationWithBlock(_forwardInvocation),
			                                       "v@:@")
			precondition(previousImpl == nil, "Swizzling DelegateProxy with other libraries is not supported.")

			classAssociations.setValue(true, forKey: delegateProxySetupKey)
		}
	}
}

extension Reactive where Base: NSObject, Base: DelegateProxyProtocol {
	/// Create a signal which sends a `next` event at the end of every
	/// invocation of `selector` on the object.
	///
	/// It completes when the object deinitializes.
	///
	/// - note: Observers to the resulting signal should not call the method
	///         specified by the selector.
	///
	/// - parameters:
	///   - selector: The selector to observe.
	///
	/// - returns: A trigger signal.
	public func trigger(for selector: Selector) -> Signal<(), NoError> {
		let base = self.base as! _DelegateProxyProtocol
		base.proxyWillIntercept(selector)

		return (self.base as NSObject).reactive.trigger(for: selector)
			.take(during: base.lifetime)
	}

	/// Create a signal which sends a `next` event, containing an array of
	/// bridged arguments, at the end of every invocation of `selector` on the
	/// object.
	///
	/// It completes when the object deinitializes.
	///
	/// - note: Observers to the resulting signal should not call the method
	///         specified by the selector.
	///
	/// - parameters:
	///   - selector: The selector to observe.
	///
	/// - returns: A signal that sends an array of bridged arguments.
	public func signal(for selector: Selector) -> Signal<[Any?], NoError> {
		let base = self.base as! _DelegateProxyProtocol
		base.proxyWillIntercept(selector)

		return (self.base as NSObject).reactive.signal(for: selector)
			.take(during: base.lifetime)
	}
}

extension Reactive where Base: NSObject {
	/// Create a transparent proxy that intercepts calls from `instance` to its delegate
	/// of the given key.
	///
	/// After the proxy is initialized, the delegate setter of `instance` would be
	/// automatically redirected to the proxy.
	///
	/// - warnings: `DelegateProxy` does not support protocols containing methods that are
	///             non-void returning. It would trap immediately when the proxy
	///             initializes with a protocol containing such a required method, or when
	///             the proxy is asked to intercept such an optional method.
	///
	/// - parameters:
	///   - key: The key of the property that stores the delegate reference.
	///
	/// - returns: The proxy.
	public func proxy<Delegate: NSObjectProtocol>(_: Delegate.Type = Delegate.self, forKey key: String) -> DelegateProxy<Delegate> {
		func remangleIfNeeded(_ name: String) -> String {
			let expression = try! NSRegularExpression(pattern: "^([a-zA-Z0-9\\_\\.]+)\\.\\(([a-zA-Z0-9\\_]+)\\sin\\s([a-zA-Z0-9\\_]+)\\)$")
			if let match = expression.firstMatch(in: name, range: NSMakeRange(0, name.characters.count)) {
				// `name` refers to a private protocol.
				let objcName = name as NSString

				let (moduleNameRange, protocolNameRange, scopeNameRange) = (match.rangeAt(1), match.rangeAt(2), match.rangeAt(3))
				let moduleName = objcName.substring(with: moduleNameRange)
				let protocolName = objcName.substring(with: protocolNameRange)
				let scopeName = objcName.substring(with: scopeNameRange)

				// Example: _TtP18ReactiveCocoaTestsP33_B2B708E0A88135A5DA71A5A2AAFA457014ObjectDelegate_
				return "_TtP\(moduleNameRange.length)\(moduleName)P\(scopeNameRange.length)\(scopeName)\(protocolNameRange.length)\(protocolName)_"
			} else if let range = name.range(of: "__ObjC.") {
				// `name` refers to an Objective-C protocol.
				return name.substring(from: range.upperBound)
			} else {
				// `name` refers to a Swift protocol.
				return name
			}
		}

		let mangledName = remangleIfNeeded(String(reflecting: Delegate.self))
		let objcProtocol = NSProtocolFromString(mangledName)!

		return base.synchronized {
			let getter = Selector(((key)))
			let setter = Selector((("set\(key.capitalized):")))

			let proxyKey = AssociationKey<AnyObject?>(setter.delegateProxyAlias)

			if let proxy = base.associations.value(forKey: proxyKey) {
				return proxy as! DelegateProxy<Delegate>
			}

			let superclass: AnyClass = class_getSuperclass(swizzleClass(base))

			let invokeSuperSetter: @convention(c) (NSObject, AnyClass, Selector, AnyObject?) -> Void = { object, superclass, selector, delegate in
				typealias Setter = @convention(c) (NSObject, Selector, AnyObject?) -> Void
				let impl = class_getMethodImplementation(superclass, selector)
				unsafeBitCast(impl, to: Setter.self)(object, selector, delegate)
			}

			let newSetterImpl: @convention(block) (NSObject, AnyObject?) -> Void = { object, delegate in
				if let proxy = object.associations.value(forKey: proxyKey) as! DelegateProxy<Delegate>? {
					proxy.forwardee = (delegate as! Delegate?)
				} else {
					invokeSuperSetter(object, superclass, setter, delegate)
				}
			}

			// Hide the original setter, and redirect subsequent delegate assignment
			// to the proxy.
			base.swizzle((setter, newSetterImpl), key: hasSwizzledKey)

			// As Objective-C classes may cache the information of their delegate at
			// the time the delegates are set, the information has to be "flushed"
			// whenever the proxy forwardee is replaced or a selector is intercepted.
			let proxy = DelegateProxy<Delegate>(objcProtocol: objcProtocol, lifetime: base.reactive.lifetime) { [weak base] proxy in
				guard let base = base else { return }
				invokeSuperSetter(base, superclass, setter, proxy)
			}

			typealias Getter = @convention(c) (NSObject, Selector) -> AnyObject?
			let getterImpl: IMP = class_getMethodImplementation(object_getClass(base), getter)
			let original = unsafeBitCast(getterImpl, to: Getter.self)(base, getter) as! Delegate?

			// `proxy.forwardee` would invoke the original setter regardless of
			// `original` being `nil` or not.
			proxy.forwardee = original

			// The proxy must be associated after it is set as the target, since
			// `base` may be an isa-swizzled instance that is using the injected
			// setters above.
			base.associations.setValue(proxy, forKey: proxyKey)

			return proxy
		}
	}
}
